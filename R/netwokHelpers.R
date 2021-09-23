#' Convert GraphML format to tidygraph
#'
#'In order to be used by ingres,
#'GraphML files have to be converted to tidygraph format. This helper does that.
#' @param filename The path to the the GraphML file.
#'
#' @return A tidygraph object
#' @export
#'
#' @examples
#' \dontrun{
#' graphmlAsTidy("network.graphml")
#' }
#'
#' # With the example file:
#' graphmlAsTidy(system.file("extdata", "network.graphml", package = "ingres"))
graphmlAsTidy = function(filename){
  tidygraph::as_tbl_graph(igraph::read_graph(filename, format = "graphml")) %>%
    filter(id != 'null')
}

#' Converts a tidygraph network into a BoolNet one.
#'
#' @param tidy.network The network to be converted.
#'
#' @return A BoolNet object.
#' @export
produceBoolnetNetwork = function(tidy.network){
  if (!requireNamespace("BoolNet", quietly = TRUE)) {
    stop("Package \"BoolNet\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  sep = ","
  hash = digest::digest(tidy.network, algo = 'xxhash32')
  tmp_file = paste0("network_", hash, ".bn")
  network.boolnet.text = tidy.network %>%
    tidygraph::activate(nodes) %>%
    mutate(
      line1 = case_when(
        kind == "input"   ~ paste(id, id, 1, sep = sep), #inputs are constant
        is.na(fixed_p) ~ paste(id, rule, 1, sep = sep), #only one function for fates or genes without NES
        kind == "gene"    ~ paste(id, rule, function_p, sep = sep)

      ),
      line2 = if_else(
        kind == "gene" & !is.na(fixed_function), #some genes may not have rules (like AP1)
        paste(id, fixed_function, fixed_p, sep = sep),
        "")) %>%
    select(line1, line2) %>%
    as.data.frame() %>% t() %>% as.vector() %>%
    stringr::str_replace_all(c("\\band\\b" = "&",
                               "\\bor\\b"  = "|",
                               "\\bnot\\b" = "!")) %>%
    purrr::discard(~ .x == "") %>%
    purrr::prepend("targets, factors, probabilities") %>%
    readr::write_lines(tmp_file)

  network = BoolNet::loadNetwork(tmp_file)
  file.remove(tmp_file)
  return(network)
}

#' Create a network genes data frame. Optionally store it as csv
#'  and open it for editing.
#' To create an ingres object, a data frame with the network nodes and
#' the corresponding gene symbols must be provided. This function simplifies
#' the process. If the gene nodes are correct gene symbols, then modification
#' is not needed and the returned data frame can be directly passed to the ingres
#' constructors.
#'
#' @param network A tidygraph network.
#' @param dir The directory where the csv will be stored, if applicable.
#' @param modify If true, store the data frame as a csv and open it to be modified
#' by the user
#'
#' @return The template data frame.
#' @export
#'

createNetworkGenesTemplate = function(network, dir = getwd(), modify = T){
  networkGenes = network %>%
    tidygraph::activate('nodes') %>%
    as_tibble() %>%
    filter(kind=='gene') %>%
    select(node = id) %>%
    mutate(symbol = node)

  if(modify){
    path = paste0(dir, "/networkGenes.csv")
    readr::write_csv(networkGenes, path)
    usethis::edit_file(path)
  }
  return(networkGenes)
}

ginmlToGraphml = function(ginzipFile){
  dest = paste0(unlist(strsplit(ginzipFile, split = ".zginml")), ".graphml")

			header = c("<?xml version=\"1.0\" encoding=\"UTF-8\"?><graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">",
			"<key attr.name=\"kind\" attr.type=\"string\" for=\"node\" id=\"kind\"/>",
			"<key attr.name=\"rule\" attr.type=\"string\" for=\"node\" id=\"rule\"/>",
			"<key attr.name=\"sign\" attr.type=\"string\" for=\"edge\" id=\"sign\"/>",
			"<graph edgedefault=\"directed\">")

	lines = header
	offset = length(lines)
  gin = unz(description = ginzipFile, filename = "GINsim-data/regulatoryGraph.ginml")

  lin =readLines(gin)
  for (i in 1:length(lin)){
    lines[[i + offset]] = lin[i] #test
  }
  close(gin)

  readr::write_lines(lines, dest)
}


#
#   String line;
#   boolean rule = false;
#   String id = null;
#   while ((line = reader.readLine()) != null) {
#     line = line.strip();
#     if (line.startsWith("<node id=")) { //start of node element
#       id = line.split("\"")[1];
#       String node = "<node id=\"" + id + "\">";
#       sj.add(node);
#     } else if (line.startsWith("<exp str=")) { //rule for node element
#       String expr = line.split("\"")[1];
#       if (!expr.equals("")) {
#         rule = true;
#         expr = expr.replaceAll("&amp;", "and");
#         expr = expr.replaceAll("\\|", "or");
#         expr = expr.replaceAll("!", "not ");
#
#         sj.add("<data key=\"rule\">" + expr + "</data>");
#       }
#     } else if (line.startsWith("</node>")) { //end of node element
#       if (rule) {
#         String kind = "gene";
#         for (Fate fate : Fate.values()) {
#           if (Objects.requireNonNull(id).equalsIgnoreCase(fate.toString())) {
#             kind = "fate";
#             break;
#           }
#         }
#         sj.add("<data key=\"kind\">" + kind + "</data>");
#       } else {
#         sj.add("<data key=\"kind\">input</data>");
#       }
#       sj.add("</node>");
#       rule = false;
#     } else if (line.startsWith("<edge id=")) { //edge node
#       Pattern p = Pattern.compile("(?<=from=\")([^\"]+).*(?<=to=\")([^\"]+).*(?<=sign=\")([^\"]+)");
#       Matcher m = p.matcher(line);
#       String from = null;
#       String to = null;
#       String sign = null;
#       while (m.find()) {
#         from = m.group(1);
#         to = m.group(2);
#         sign = m.group(3);
#       }
#       sj.add("<edge source=\"" + from + "\" target=\"" + to + "\">");
#       sj.add("<data key=\"sign\">" + sign + "</data>");
#       sj.add("</edge>");
#
#     }
#   }
#   sj.add("</graph>");
#   sj.add("</graphml>");
#   String graphml = sj.toString();
#   var writer = new BufferedWriter(new FileWriter(dest));
#   writer.write(graphml);
#   writer.close();
# }
