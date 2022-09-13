setOldClass("tbl_graph")

#' The ingres class.
#'
#' @aliases ingresClass ingresObject
#' @slot expression matrix.
#' @slot idents data.frame.
#' @slot network.genes data.frame.
#' @slot viper data.frame.
#' @slot cluster.pbn data.frame.
#' @slot single.cell.pbn data.frame.
#' @slot network \code{tbl_graph}.
#'
#' @return An \code{ingres} object.
ingres = setClass("ingres", slots = list(
  expression = "matrix",
  idents = "data.frame",
  network.genes = "data.frame",
  viper = "data.frame",
  cluster.pbn = "data.frame",
  single.cell.pbn = "data.frame",
  network = "tbl_graph"
))

print.ingres = function(object) {
  ncells = ncol(object@expression)
  nfeat = nrow(object@expression)
  nclusters = length(unique(object@idents$cluster))
  nnodes = object@network %>%
    tidygraph::activate("nodes") %>%
    length()
  viper = nrow(object@viper) > 0
  pbnCell = nrow(object@single.cell.pbn) > 0
  pbnCluster = nrow(object@cluster.pbn) > 0

  cat("An ingres object\n")
  cat("--------------------\n")
  cat(nfeat, "features across", ncells, "samples\n")
  cat(nclusters, "clusters\n")
  cat("Its Boolean network has", nnodes, "nodes\n")
  cat("Viper", ifelse(viper, "performed", "not performed"), "\n")
  cat(ifelse(pbnCell, "PBNs computed for each cell\n", ""))
  cat(ifelse(pbnCluster, "PBNs computed for each cluster", ""))
}

setMethod("show", "ingres", print.ingres)

#' Create a new \code{ingres} object using the expression data in a
#'  \code{Seurat} object.
#'
#' @param seurat.object A \code{Seurat} object with QC, scaling and
#' clustering already executed.
#' @param seurat.assay The name of the assay to be imported,
#' normally 'RNA' or 'Spatial'.
#' @param slot The name of the slot that contains the matrix to be used.
#' @param network.genes A data frame with a column for node names
#' and another column for the corresponding entrez IDs.
#' @param network A object of class \code{tbl_graph} representing
#' our network of interest.
#'
#' @return An \code{ingres} object.
#'
#' @examples
#' createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#'
#' @export
createIngresObjectFromSeurat = function(seurat.object, seurat.assay = "RNA",
                                        slot = "data", network.genes, network) {
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "Package \"Seurat\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  expression.matrix = as.matrix(
    Seurat::GetAssayData(seurat.object, assay = seurat.assay, slot = slot)
  )
  idents = data.frame(
    cell = names(Seurat::Idents(seurat.object)),
    cluster = as.character(Seurat::Idents(seurat.object)),
    check.names = FALSE
  )
  createIngresObject(expression.matrix, idents, network.genes, network)
}

#' Create a new \code{ingres} object.
#'
#' @param expression.matrix A single-cell expression matrix,
#'  with cells in rows and genes in columns.
#' @param idents A data frame with a column for cell barcode
#' and another column for the corresponding cluster or subpopulation.
#' @param network.genes A data frame with a column for node names
#' and another column for the corresponding entrez IDs.
#' @param network A object of class \code{tbl_graph} representing
#'  our network of interest.
#'
#' @return An \code{ingres} object.
#'
#' @examples
#' # Get expression matrix from small_blca_wang for convenience, but it can be
#' # any single-cell expression matrix, from any source. Same for idents.
#' expression.matrix =
#'   as.matrix(Seurat::GetAssayData(
#'     small_blca_wang,
#'     assay = "RNA", slot = "data"
#'   ))
#'
#' idents = data.frame(
#'   cell = names(Seurat::Idents(small_blca_wang)),
#'   cluster = as.character(Seurat::Idents(small_blca_wang)),
#'   check.names = FALSE
#' )
#'
#' ing = createIngresObject(expression.matrix, idents, network_genes, network)
#' ing
#'
#' @export
createIngresObject =
  function(expression.matrix, idents, network.genes, network) {
    ingres(
      expression = expression.matrix, idents = idents,
      network.genes = network.genes, network = network
    )
  }
