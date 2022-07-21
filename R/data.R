#' An example dataframe with the genes contained in the network,
#' in the format required by the package
"network_genes"

#' An example network,
#' in the format required by the package.
#' The network is a modified version of the one in
#' Remy et al. Cancer Res 75, 4042–4052 (2015)
"network"

#' An example small Seurat object, using dataset GSE130001 from
#' Wang et al. Genome Med. 2020 Dec;12(1):24.
"small_blca_wang"

#' The results of running [performViper()] on an ingres object created with
#' `small_blca_wang`, intended to speed up vignettes and examples.
#' This data frame was subset to only contain the genes in the `network_genes`
#' file, so to keep the file size small.
"viper_results"
