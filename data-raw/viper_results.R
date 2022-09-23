## code to prepare `viper_results` dataset goes here
ing <- createIngresObjectFromSeurat(
  small_blca_wang, "RNA", "data", network_genes, network
) %>%
  performViper(aracne.networks::regulonblca, verbose = F)

viper_results <- ing@viper

# keep only the genes in the network to make the file smaller
viper_results <- viper_results %>%
  select(c(cell, cluster) | any_of(network_genes$symbol))
usethis::use_data(viper_results, overwrite = TRUE, compress = "xz")
