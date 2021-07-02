setOldClass("tbl_graph")

#' The ingres class.
#'
#' @slot expression matrix.
#' @slot idents data.frame.
#' @slot network.genes data.frame.
#' @slot viper data.frame.
#' @slot cluster.pbn data.frame.
#' @slot single.cell.pbn data.frame.
#' @slot network \code{tbl_graph}.
#'
#' @return An \code{ingres} object.
#' @export
ingres = setClass("ingres", slots = list(expression = "matrix",
                                       idents = "data.frame",
                                       network.genes = "data.frame",
                                       viper = "data.frame",
                                       cluster.pbn = "data.frame",
                                       single.cell.pbn = "data.frame",
                                       network = "tbl_graph"))

#' Create a new \code{ingres} object using the expression data in a \code{Seurat} object.
#'
#' @param seurat.object A \code{Seurat} object with QC, scaling and clustering already executed.
#' @param seurat.assay The name of the assay to be imported, normally 'RNA' or 'Spatial'.
#' @param slot The name of the slot that contains the matrix to be used.
#' @param network.genes A data frame with a column for node names and another column for the corresponding entrez IDs.
#' @param network A object of class \code{tbl_graph} representing our network of interest.
#'
#' @return An \code{ingres} object.
#' @export
createIngresObjectFromSeurat = function(seurat.object, seurat.assay, slot, network.genes, network){
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Package \"Seurat\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  expression.matrix = as.matrix(Seurat::GetAssayData(seurat.object, assay = seurat.assay, slot = "scale.data"))
  idents = data.frame(cell = names(Seurat::Idents(seurat.object)),
                      cluster = as.character(Seurat::Idents(seurat.object)),
                      check.names = F)
  createIngresObject(expression.matrix, idents, network.genes, network)
}

#' Create a new \code{ingres} object.
#'
#' @param expression.matrix A single-cell expression matrix, with cells in rows and genes in columns.
#' @param idents A data frame with a column for cell barcode and another column for the corresponding cluster or subpopulation.
#' @param network.genes A data frame with a column for node names and another column for the corresponding entrez IDs.
#' @param network A object of class \code{tbl_graph} representing our network of interest.
#'
#' @return An \code{ingres} object.
#' @export
createIngresObject = function(expression.matrix, idents, network.genes, network){
  ingres(expression = expression.matrix, idents = idents, network.genes = network.genes, network = network)
}
