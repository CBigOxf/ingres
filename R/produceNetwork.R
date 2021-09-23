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


