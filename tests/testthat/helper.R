createExampleIngresObject = function(){
  obj = createIngresObjectFromSeurat(small_blca_wang,
                                     "RNA", "data",
                                     network_genes,
                                     network)
}

createExampleIngresObjectAndPerformViper = function(){
  createExampleIngresObject() %>%
    performViper(aracne.networks::regulonblca, verbose = F)
}

#create a object shared by all tests so it only needs to be created once,
#except when testing the creation of the object itself.
ingresAfterViper = createExampleIngresObjectAndPerformViper()

# Load all regulons in aracne.networks
aracneRegulons = function(){
  c(aracne.networks::regulonblca, aracne.networks::regulonbrca,
    aracne.networks::reguloncesc, aracne.networks::reguloncoad,
    aracne.networks::regulonesca, aracne.networks::regulongbm,
    aracne.networks::regulonhnsc, aracne.networks::regulonkirc,
    aracne.networks::regulonkirp, aracne.networks::regulonlaml,
    aracne.networks::regulonlihc, aracne.networks::regulonluad,
    aracne.networks::regulonlusc, aracne.networks::regulonnet,
    aracne.networks::regulonov, aracne.networks::regulonpaad,
    aracne.networks::regulonpcpg, aracne.networks::regulonprad,
    aracne.networks::regulonread, aracne.networks::regulonsarc,
    aracne.networks::regulonstad, aracne.networks::regulontgct,
    aracne.networks::regulonthca, aracne.networks::regulonthym,
    aracne.networks::regulonucec)
}
