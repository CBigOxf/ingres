createExampleIngresObject = function() {
  obj = createIngresObjectFromSeurat(
    small_blca_wang,
    "RNA", "data",
    network_genes,
    network
  )
}

createExampleIngresObjectWithViper = function() {
  ing = createExampleIngresObject()
  ing@viper = viper_results
  return(ing)
}

ingresAfterViper = createExampleIngresObjectWithViper()
