#' Perform VIPER
#'
#'Performs VIPER analysis on a RNA-seq dataset contained by an ingres object.
#'This function is merely a wrapper around the \code{\link[viper:viper]{original VIPER function}},
#'designed to suit the ingres workflow.
#' @param ingres.object An ingres object.
#' @inheritParams viper::viper
#' @return An ingres object with the result of the VIPER inference in the \code{viper} slot
#' @export
performViper = function(ingres.object, regulon){
  expression.matrix = ingres.object@expression
  #translate gene symbols to entrezid
  geneIds = AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                  keys = rownames(expression.matrix),
                                  column="ENTREZID", keytype="SYMBOL",
                                  multiVals = "first")

  notFound = geneIds[which(is.na(geneIds))]
  geneIds = geneIds[which(!is.na(geneIds))] #remove those not found
  expression.matrix = expression.matrix[names(geneIds),] #subsetting to found genes
  rownames(expression.matrix) = geneIds #renaming rows as entrez ids

  #run viper on matrix using provided regulon/s
  viperMatrix = viper::viper(expression.matrix, regulon)
  gc() #it's a good idea to run the garbage collector now to free up memory
  network.genes = AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                        keys = ingres.object@network.genes$symbol,
                                        column="ENTREZID", keytype="SYMBOL",
                                        multiVals = "first")
  viperSubsetMatrix =
    t(subset(viperMatrix, rownames(viperMatrix) %in% network.genes))

  #translate back to gene symbols
  geneSymbols = AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                      keys = colnames(viperSubsetMatrix),
                                      column="SYMBOL", keytype="ENTREZID",
                                      multiVals = "first")
  colnames(viperSubsetMatrix) = geneSymbols

  #add cluster column to viper results
  viperSubsetDf = as.data.frame(viperSubsetMatrix)
  viperSubsetDf = cbind(cell = rownames(viperSubsetDf),
                        viperSubsetDf, row.names = NULL) #cell id to new column
  viper.result = merge(ingres.object@idents, viperSubsetDf)
  ingres.object@viper = viper.result
  ingres.object
}