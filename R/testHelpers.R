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
  data("small_blca_wang")
  data("network")
  data("network_genes")
  obj <- createIngresObjectFromSeurat(
    small_blca_wang,
    "RNA", "data",
    network_genes,
    network
  )
}

#' @rdname createExampleIngresObject
createExampleIngresObjectWithViper <- function() {
  ing <- createExampleIngresObject()
  data("viper_results")
  ing@viper <- viper_results
  return(ing)
}

ingresAfterViper <- createExampleIngresObjectWithViper()
