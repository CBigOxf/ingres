#' Create a toy \code{\linkS4class{ingres}} object to use in examples and tests.
#' @description \code{createExampleIngresObjectWithViper additionally
#' fills the viper slot with a dummy viper object}
#'
#' @return An \code{\linkS4class{ingres}} object
#' @export
#'
#' @examples
#' createExampleIngresObject()
#' createExampleIngresObjectWithViper()
createExampleIngresObject <- function() {
  obj <- createIngresObjectFromSeurat(
    ingres::small_blca_wang,
    "RNA", "data",
    ingres::network_genes,
    ingres::network
  )
}

#' @rdname createExampleIngresObject
createExampleIngresObjectWithViper <- function() {
  ing <- createExampleIngresObject()
  ing@viper <- ingres::viper_results
  return(ing)
}

ingresAfterViper <- createExampleIngresObjectWithViper()
