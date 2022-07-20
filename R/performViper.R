#' Perform VIPER
#'
#' Performs VIPER analysis on a RNA-seq dataset contained by an ingres object.
#' This function is merely a wrapper around the \code{\link[viper:viper]{original VIPER function}},
#' designed to suit the ingres workflow and properly prepare the input data.
#' @param ingres.object An ingres object.
#' @inheritParams viper::viper
#' @return An ingres object with the result of the VIPER inference in the \code{viper} slot
#' @export
performViper = function(ingres.object, regulon, verbose = T) {
  expression.matrix = ingres.object@expression
  # translate gene symbols to entrezid
  geneIds = suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
    keys = rownames(expression.matrix),
    column = "ENTREZID", keytype = "SYMBOL",
    multiVals = "first"
  ))

  geneIds = geneIds[which(!is.na(geneIds))] # remove those not found
  expression.matrix = expression.matrix[names(geneIds), ] # subsetting to found genes
  rownames(expression.matrix) = geneIds # renaming rows as entrez ids

  # run viper on matrix using provided regulon/s
  viperMatrix = viper::viper(expression.matrix, regulon, verbose = verbose)
  gc() # it's a good idea to run the garbage collector now to free up memory

  viperMatrix %<>% t()

  # translate back to gene symbols
  geneSymbols = suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
    keys = colnames(viperMatrix),
    column = "SYMBOL", keytype = "ENTREZID",
    multiVals = "first"
  ))
  colnames(viperMatrix) = geneSymbols

  # add cluster column to viper results
  viperDf = as.data.frame(viperMatrix)
  viperDf = cbind(
    cell = rownames(viperDf),
    viperDf, row.names = NULL
  ) # cell id to new column
  viper.result = merge(ingres.object@idents, viperDf)
  ingres.object@viper = viper.result
  ingres.object
}
