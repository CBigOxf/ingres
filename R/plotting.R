#' Plot a cell PBN
#'
#' Plots the PBN produced by \code{\link{computePbnBySingleCell}} and returns it as
#' a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{single.cell.pbn slot} slot.
#' @param cell.id A string containing the id of the cell to be added.
#'
#' @return A \code{ggplot} object containing the cell PBN plot.
#' @export
cellPbnPlot = function(ingres.object, cell.id){
  p.cell = ingres.object@single.cell.pbn %>%
    filter(cell == cell.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "label", values_to = "p")

  networkPlot(ingres.object@network, p.cell, paste0("PBN for cell ", cell.id))
}

#' Plot a cluster PBN
#'
#' Plots the PBN produced by \code{\link{computePbnByCluster}} and returns it as
#' a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{cluster.pbn slot} slot.
#' @param cluster.id A string containing the id of the cluster to be added.
#'
#' @return A \code{ggplot} object containing the cluster PBN plot.
#' @export
clusterPbnPlot = function(ingres.object, cluster.id){
  p.cluster = ingres.object@cluster.pbn %>%
    filter(cluster == cluster.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "label", values_to = "p")

  networkPlot(ingres.object@network, p.cluster, paste0("PBN for cluster ", cluster.id))
}

#for internal use only
networkPlot = function(network, pbn.df, title){
  plot.data = network %>%
    tidygraph::activate(nodes) %>%
    left_join(pbn.df, by="label") %>%
    mutate(p.sign = as.character(sign(p)), p = abs(round(p/1000, digits = 2))) %>%
    mutate(label = stringr::str_replace(label, "_", "\n")) %>%
    mutate(label.p = paste0(label, "\np=",p))

  p = ggraph(plot.data, layout = "stress") +
    geom_edge_fan2(aes(edge_colour = sign),
                   start_cap = circle(12, 'mm'), end_cap = circle(12, "mm"),
                   arrow = arrow(angle = 30, length = unit(3, "mm"),
                                 ends = "last", type = "open")) +
    geom_node_point(aes(fill = p.sign, shape = kind), size = 25) +
    geom_node_text(aes(filter = is.na(p), label = label)) +
    geom_node_text(aes(filter = !is.na(p), label = label.p)) +
    scale_fill_manual(values = c("#DB1F48", "#01949A", "#004369")) +
    scale_shape_manual(values = c(22, 21, 23)) +
    scale_edge_width(range = c(0.2,3)) +
    theme_graph() +
    theme(legend.position = "none") +
    ggtitle(title)
  p
}
