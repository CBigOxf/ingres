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

# Save a ggplot returned by code and return the path.
save_gg = function(code, width = 4, height = 2) {
  path = tempfile(fileext = ".png")
  ggsave(path, plot = code, width = width, height = height)
  path
}

# skips plot snapshot testing when not reproducible.
expect_snapshot_plot = function(name, code, width = 4, height = 2) {
  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  name = paste0(name, ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path = save_gg(code, width, height)
  expect_snapshot_file(path, name)
}

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
