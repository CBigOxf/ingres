#' Convert GraphML format to tidygraph
#'
#' In order to be used by ingres,
#' GraphML files have to be converted to tidygraph format. This helper does that.
#' @param filename The path to the the GraphML file.
#'
#' @return A tidygraph object
#'
#' @examples
#' filename =
#'   system.file("extdata", "example_network.graphml", package = "ingres")
#' graphmlAsTidy(filename)
#'
#' @export
graphmlAsTidy = function(filename) {
  tidygraph::as_tbl_graph(igraph::read_graph(filename, format = "graphml")) %>%
    filter(id != "null")
}

#' Convert an ingres network into a BoolNet one
#'
#' @param network The network to be converted, created
#'  by [produceNetworkForCluster()] or [produceNetworkForCell()]
#'
#' @return A BoolNet object.
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' # Compute PBNs by cluster
#' ing = computePbnByCluster(ing)
#'
#' # Produce a network for an arbitrary cluster
#' network = produceNetworkForCluster(ing, "1")
#'
#' produceBoolnetNetwork(network)
#'
#' @export
produceBoolnetNetwork = function(network) {
  if (!requireNamespace("BoolNet", quietly = TRUE)) {
    stop("Package \"BoolNet\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  sep = ","
  tmp_file = withr::local_tempfile(fileext = ".bn")
  network.boolnet.text = network %>%
    tidygraph::activate("nodes") %>%
    mutate(
      line1 = case_when(
        # inputs are constant
        .data$kind == "input" ~ paste(id, id, 1, sep = sep),
        # only one function for fates or genes without NES
        is.na(.data$fixed_p) ~ paste(id, rule, 1, sep = sep),
        .data$kind == "gene" ~ paste(id, rule, function_p, sep = sep)
      ),
      line2 = if_else(
        # some genes may not have rules (like AP1)
        .data$kind == "gene" & !is.na(.data$fixed_function),
        paste(id, .data$fixed_function, .data$fixed_p, sep = sep),
        ""
      )
    ) %>%
    select(.data$line1, .data$line2) %>%
    as.data.frame() %>%
    t() %>%
    as.vector() %>%
    stringr::str_replace_all(c(
      "\\band\\b" = "&",
      "\\bor\\b" = "|",
      "\\bnot\\b" = "!"
    )) %>%
    purrr::discard(~ .x == "") %>%
    purrr::prepend("targets, factors, probabilities") %>%
    readr::write_lines(tmp_file)

  network = BoolNet::loadNetwork(tmp_file)
  return(network)
}

#' Create a network genes data frame. Optionally store it as csv
#' and open it for editing
#' To create an ingres object, a data frame with the network nodes and
#' the corresponding gene symbols must be provided. This function simplifies
#' the process. If the gene nodes are correct gene symbols, then modification
#' is not needed and the returned data frame can be directly passed to
#' the ingres constructors.
#'
#' @param network A tidygraph network.
#' @param dir The directory where the csv will be stored, if applicable.
#' @param store If true, store the data frame as a csv
#' @param modify If true, and store is also true,
#'  open it to be modified by the user
#' @return The template data frame.
#'
#' @examples
#' tmp = tempdir()
#' createNetworkGenesTemplate(network, dir = tmp)
#' file.remove(paste(tmp, "/networkGenes.csv")) # cleanup
#'
#' @export
createNetworkGenesTemplate = function(network, dir = getwd(),
                                      store = TRUE, modify = TRUE) {
  networkGenes = network %>%
    tidygraph::activate("nodes") %>%
    as_tibble() %>%
    filter(.data$kind == "gene") %>%
    select(node = id) %>%
    mutate(symbol = .data$node)

  if (store) {
    path = paste0(dir, "/networkGenes.csv")
    readr::write_csv(networkGenes, path)
    if (modify) {
      usethis::edit_file(path)
    }
  }
  return(networkGenes)
}

#' Convert a GinSim file into a GraphML file
#' GinSim files have the extension .zginml. This utility function converts such
#' files into the GraphML format. keeping the kind - fate, input or gene -, the
#' edge sign and the rule - formulae - data.
#' @param ginzipFile The path to the .zginml file. The GraphML file will be
#' created in the same directory.
#' @param fates A vector of fate names. If left empty, both fate and gene nodes
#' will be labelled as kind='gene'
#' @param dest The path to the graphml file that will be created. If NULL,
#' defaults to the same path as the zginml file, but with the graphml extension.
#' @return A vector with the lines of the newly created GraphML file.
#'
#' @examples
#' filename =
#'   system.file("extdata", "example_ginsim.zginml", package = "ingres")
#' head(ginmlToGraphml(filename))
#'
#' @export
ginmlToGraphml = function(ginzipFile, fates = c(), dest = NULL) {
  if (is.null(dest)) {
    dest = paste0(unlist(strsplit(ginzipFile, split = ".zginml")), ".graphml")
  }

  header = c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">",
    "<key attr.name=\"kind\" attr.type=\"string\" for=\"node\" id=\"kind\"/>",
    "<key attr.name=\"rule\" attr.type=\"string\" for=\"node\" id=\"rule\"/>",
    "<key attr.name=\"sign\" attr.type=\"string\" for=\"edge\" id=\"sign\"/>",
    "<graph edgedefault=\"directed\">"
  )

  result = c()
  gin = unz(
    description = ginzipFile,
    filename = "GINsim-data/regulatoryGraph.ginml"
  )
  rule = F
  id = ""

  lin = readLines(gin)
  for (i in 1:length(lin)) {
    line = stringr::str_squish(lin[i])
    if (stringr::str_starts(line, "<node id=")) { # node start tag
      id = strsplit(line, "\"", fixed = T)[[1]][2]
      nodeTag = paste0("<node id=\"", id, "\">")
      result[length(result) + 1] = nodeTag
    } else if (stringr::str_starts(line, "<exp str=")) { # rule tag
      expr = strsplit(line, "\"", fixed = T)[[1]][2]
      rule = T
      expr = stringr::str_replace_all(
        expr,
        c("&amp;" = "and", "\\|" = "or", "!" = "not ")
      )
      ruleTag = paste0("<data key=\"rule\">", expr, "</data>")
      result[length(result) + 1] = ruleTag
    } else if (stringr::str_starts(line, "</node>")) { # node end tag
      kind = "input"
      if (rule) {
        if (id %in% fates) {
          kind = "fate"
        } else {
          kind = "gene"
        }
      }
      kindTag = paste0("<data key=\"kind\">", kind, "</data>")
      endTag = "</node>"
      result = append(result, c(kindTag, endTag), after = length(result))
      rule = F
    } else if (stringr::str_starts(line, "<edge id=")) { # edge tag
      pattern = "(?<=from=\")([^\"]+).*(?<=to=\")([^\"]+).*(?<=sign=\")([^\"]+)"
      matches = stringr::str_match(line, pattern)
      from = matches[1, 2]
      to = matches[1, 3]
      sign = matches[1, 4]
      edgeTag = paste0("<edge source=\"", from, "\" target=\"", to, "\">")
      signTag = paste0("<data key=\"sign\">", sign, "</data>")
      edgeEndTag = "</edge>"
      result = append(result, c(edgeTag, signTag, edgeEndTag),
        after = length(result)
      )
    }
  }
  close(gin)
  result = append(result, header, 0)
  result = append(result, c("</graph>", "</graphml>"), after = length(result))
  readr::write_lines(result, dest)
  return(result)
}


#' Print all nodes in a network

#' For testing and checks purposes.
#'
#' @param network The network which nodes will be printed.
#'
#' @return No return value, called only to print into the output.
#'
#' @examples
#' printAllNodes(network)
#'
#' @export
printAllNodes = function(network) {
  tb = network %>%
    tidygraph::activate("nodes") %>%
    tidygraph::as_tibble()
  le = nrow(tb)
  print(tb, n = le)
}
