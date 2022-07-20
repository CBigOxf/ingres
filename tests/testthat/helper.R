createExampleIngresObject = function() {
  obj = createIngresObjectFromSeurat(
    small_blca_wang,
    "RNA", "data",
    network_genes,
    network
  )
}

createExampleIngresObjectAndPerformViper = function() {
  createExampleIngresObject() %>%
    performViper(aracne.networks::regulonblca, verbose = F)
}

# create a object shared by all tests so it only needs to be created once,
# except when testing the creation of the object itself.
ingresAfterViper = createExampleIngresObjectAndPerformViper()
