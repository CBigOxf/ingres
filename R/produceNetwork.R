#' Produce a tidygraph network for a given cluster.
#'
#' @param ingres.object The ingres object containing the relevant data.
#' @param cluster.id The id of the cluster for which the network is to be produced.
#'
#' @return A tidygraph object containing the network for the given cluster.
#' @export
produceNetworkForCluster = function(ingres.object, cluster.id){
  pbn.data = ingres.object@cluster.pbn %>%
    filter(cluster == cluster.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "id", values_to = "p")

 produceNetwork(ingres.object@network, pbn.data)
}

#' Produce a tidygraph network for a given cell.
#'
#' @param ingres.object The ingres object containing the relevant data.
#' @param cell.id The id of the cell for which the network is to be produced.
#'
#' @return A tidygraph object containing the network for the given cell.
#' @export
produceNetworkForCell = function(ingres.object, cell.id){
  pbn.data = ingres.object@single.cell.pbn %>%
    filter(cell == cell.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "id", values_to = "p")

  produceNetwork(ingres.object@network, pbn.data)
}

# for internal use only
produceNetwork = function(network, pbn.data){
  network = network %>%
    tidygraph::activate(nodes) %>%
    left_join(pbn.data, by="id") %>%
    mutate(fixed_p = ifelse(p == 0,
                            NA, #don't create a fixed function if p==0
                            abs(p))) %>%
    mutate(fixed_function = ifelse(p > 0, 1, 0)) %>%
    mutate(function_p = 1 - fixed_p)
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
    activate(nodes) %>%
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
    stringr::  str_replace_all(c("\\band\\b" = "&",
                                 "\\bor\\b"  = "|",
                                 "\\bnot\\b" = "!")) %>%
    purrr::discard(~ .x == "") %>%
    purrr::prepend("targets, factors, probabilities") %>%
    readr::write_lines(tmp_file)

  network = BoolNet::loadNetwork(tmp_file)
  file.remove(tmp_file)
  return(network)
}


