#' Plot a cell PBN
#'
#' Plots the PBN produced by \code{\link{computePbnBySingleCell}} for a given cell
#' and returns it as a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{single.cell.pbn} slot.
#' @param cell.id A string containing the id of the cell to be plotted.
#'
#' @return A \code{ggplot} object containing the cell PBN plot.
#' @export
cellPbnPlot = function(ingres.object, cell.id){
  p.cell = ingres.object@single.cell.pbn %>%
    filter(cell == cell.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "id", values_to = "p")

  networkPlot(ingres.object@network, p.cell, paste0("PBN for cell ", cell.id))
}

#' Plot a cluster PBN
#'
#' Plots the PBN produced by \code{\link{computePbnByCluster}} for a given cluster
#' and returns it as a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{cluster.pbn} slot.
#' @param cluster.id A string containing the id of the cluster to be plotted.
#'
#' @return A \code{ggplot} object containing the cluster PBN plot.
#' @export
clusterPbnPlot = function(ingres.object, cluster.id){
  p.cluster = ingres.object@cluster.pbn %>%
    filter(cluster == cluster.id) %>%
    select(-c(1,2)) %>%
    pivot_longer(tidyselect::everything(), names_to = "id", values_to = "p")

  networkPlot(ingres.object@network, p.cluster, paste0("PBN for cluster ", cluster.id))
}

#for internal use only
networkPlot = function(network, pbn.df, title){
  plot.data = network %>%
    tidygraph::activate(nodes) %>%
    left_join(pbn.df, by="id") %>%
    mutate(p.sign = as.character(sign(p)), p = abs(round(p/1000, digits = 2))) %>%
    mutate(id = stringr::str_replace(id, "_", "\n")) %>%
    mutate(id.p = paste0(id, "\np=",p))

  p = ggraph(plot.data, layout = "stress") +
    geom_edge_fan2(aes(edge_colour = sign),
                   start_cap = circle(12, 'mm'), end_cap = circle(12, "mm"),
                   arrow = arrow(angle = 30, length = unit(3, "mm"),
                                 ends = "last", type = "open")) +
    geom_node_point(aes(fill = p.sign, shape = kind), size = 25) +
    geom_node_text(aes(filter = is.na(p), label = id)) +
    geom_node_text(aes(filter = !is.na(p), label = id.p)) +
    scale_fill_manual(values = c("#DB1F48", "#01949A", "#004369")) +
    scale_shape_manual(values = c(22, 21, 23)) +
    scale_edge_width(range = c(0.2,3)) +
    theme_graph() +
    theme(legend.position = "none") +
    ggtitle(title)
  p
}

#' Plot a cluster heatmap
#'
#' Plots the probabilities for the identity function produced by \code{\link{computePbnByCluster}}
#' as a heatmap, with nodes on the x axis and clusters on the y axis, and returns
#' it as a ggplot object.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{cluster.pbn} slot.
#'
#' @return A \code{ggplot} object containing the cluster PBN heatmap plot.
#' @export
clusterGenesHeatmap = function(ingres.object){
  p = ingres.object@cluster.pbn %>%
    select(-2) %>%
    pivot_longer(!cluster, names_to = "node", values_to = "p") %>%
    mutate(p = p/1000) %>%
    ggplot(aes(x=node, y=cluster)) +
    geom_raster(aes(fill=p)) +
    scale_fill_continuous(low = "violetred3", high = "aquamarine2") +
    ggpubr::theme_pubr() +
    theme(axis.text.x = element_text(angle=-45, vjust = 1, hjust = 0))
  p
}

#' Plot a cell heatmap
#'
#' Plots the probabilities for the identity function produced by \code{\link{computePbnBySingleCell}}
#' as a heatmap, with nodes on the x axis and cells on the y axis, grouped by cluster,
#' and returns it as a ggplot object.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null \code{single.cell.pbn} slot.
#'
#' @return A \code{ggplot} object containing the cell PBN heatmap plot.
#' @export
cellGenesHeatmap = function(ingres.object){
  p = ingres.object@single.cell.pbn %>%
    pivot_longer(!c(cell, cluster), names_to = "node", values_to = "p") %>%
    mutate(p = p/1000, clustern = substring(cluster, first = 1, last = 20)) %>%
    ggplot(aes(x=node, y=cell)) +
    geom_tile(aes(fill=p)) +
    scale_fill_continuous(low = "violetred3", high = "aquamarine2") +
    ggpubr::theme_pubr(legend = "none") +
    theme(axis.text.x = element_text(angle=-45, vjust = 1, hjust = 0),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    facet_grid(clustern ~ ., scales = "free")
  p
}
