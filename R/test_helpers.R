createExampleIngresObject = function(){
  obj = createIngresObjectFromSeurat(pbmc_small,
                                     "RNA", "data",
                                     network_genes,
                                     network)
}

createExampleIngresObjectWithViper = function(){
  obj = createExampleIngresObject()
  obj@viper = viperResults
  obj
}
