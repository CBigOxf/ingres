#' Plot a cell PBN
#'
#' Plots the PBN produced by \code{\link{computePbnBySingleCell}} for a
#' given cell and returns it as a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null
#' \code{single.cell.pbn} slot.
#' @param cell.id A string containing the id of the cell to be plotted.
#'
#' @return A \code{ggplot} object containing the cell PBN plot.
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' ing = computePbnBySingleCell(ing)
#' cellPbnPlot(ing, "sample1@ACAGCTAAGATCCCGC-1")
#'
#' @export
cellPbnPlot = function(ingres.object, cell.id) {
  networkPlot(
    produceNetworkForCell(ingres.object, cell.id),
    paste0("PBN for cell ", cell.id)
  )
}

#' Plot a cluster PBN
#'
#' Plots the PBN produced by \code{\link{computePbnByCluster}} for a given
#' cluster and returns it as a ggplot object
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null
#' \code{cluster.pbn} slot.
#' @param cluster.id A string containing the id of the cluster to be plotted.
#'
#' @return A \code{ggplot} object containing the cluster PBN plot.
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' ing = computePbnByCluster(ing)
#' clusterPbnPlot(ing, "1")
#'
#' @export
clusterPbnPlot = function(ingres.object, cluster.id) {
  networkPlot(
    produceNetworkForCluster(ingres.object, cluster.id),
    paste0("PBN for cluster ", cluster.id)
  )
}

# for internal use only
networkPlot = function(network, title) {
  plot.data = network %>%
    tidygraph::activate("nodes") %>%
    mutate(print.id = stringr::str_replace(id, "_", "\n")) %>%
    mutate(
      print.id.p =
        paste0(.data$print.id, "\np=", round(.data$fixed_p, digits = 2))
    )

  p = ggraph(plot.data, layout = "stress") +
    geom_edge_fan2(aes(edge_colour = sign),
      start_cap = circle(12, "mm"), end_cap = circle(12, "mm"),
      arrow = arrow(
        angle = 30, length = unit(3, "mm"),
        ends = "last", type = "open"
      )
    ) +
    geom_node_point(aes(
      fill = as.factor(.data$fixed_function),
      shape = .data$kind
    ), size = 25) +
    geom_node_text(aes(
      filter = is.na(.data$fixed_p), label = .data$print.id
    )) +
    geom_node_text(aes(
      filter = !is.na(.data$fixed_p), label = .data$print.id.p
    )) +
    scale_fill_manual(values = c("#DB1F48", "#01949A", "#004369")) +
    scale_shape_manual(values = c(22, 21, 23)) +
    scale_edge_width(range = c(0.2, 3)) +
    theme_graph(base_family = "sans") +
    theme(legend.position = "none") +
    ggtitle(title)
  p
}

#' Plot a cluster heatmap
#'
#' Plots the probabilities for the identity function produced by
#' \code{\link{computePbnByCluster}}
#' as a heatmap, with nodes on the x axis and clusters on the y axis, and
#' returns it as a ggplot object.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null
#' \code{cluster.pbn} slot.
#'
#' @return A \code{ggplot} object containing the cluster PBN heatmap plot.
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' ing = computePbnByCluster(ing)
#' clusterGenesHeatmap(ing)
#'
#' @export
clusterGenesHeatmap = function(ingres.object) {
  p = ingres.object@cluster.pbn %>%
    select(-2) %>%
    pivot_longer(!.data$cluster, names_to = "node", values_to = "p") %>%
    ggplot(aes(x = .data$node, y = .data$cluster)) +
    geom_raster(aes(fill = p)) +
    scale_fill_continuous(low = "violetred3", high = "aquamarine2") +
    ggpubr::theme_pubr() +
    theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))
  p
}

#' Plot a cell heatmap
#'
#' Plots the probabilities for the identity function produced by
#' \code{\link{computePbnBySingleCell}} as a heatmap, with nodes on the x axis
#'  and cells on the y axis, grouped by cluster, and returns it as a
#'  ggplot object.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null
#' \code{single.cell.pbn} slot.
#'
#' @return A \code{ggplot} object containing the cell PBN heatmap plot.
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' ing = computePbnBySingleCell(ing)
#' cellGenesHeatmap(ing)
#'
#' @export
cellGenesHeatmap = function(ingres.object) {
  p = ingres.object@single.cell.pbn %>%
    pivot_longer(!c(.data$cell, .data$cluster),
      names_to = "node", values_to = "p"
    ) %>%
    mutate(clustern = substring(.data$cluster, first = 1, last = 20)) %>%
    ggplot(aes(x = .data$node, y = .data$cell)) +
    geom_tile(aes(fill = p)) +
    scale_fill_continuous(low = "violetred3", high = "aquamarine2") +
    ggpubr::theme_pubr(legend = "none") +
    theme(
      axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    ) +
    facet_grid(clustern ~ ., scales = "free")
  p
}
