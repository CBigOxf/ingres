## code to prepare `viper_results` dataset goes here
ing = createIngresObjectFromSeurat(
  small_blca_wang, "RNA", "data", network_genes, network
) %>%
  performViper(aracne.networks::regulonblca, verbose = F)

viper_results = ing@viper
usethis::use_data(viper_results, overwrite = TRUE, compress = "xz")
