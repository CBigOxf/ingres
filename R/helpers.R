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
  tidygraph::as_tbl_graph(igraph::read_graph(filename, format = "graphml"))
}
