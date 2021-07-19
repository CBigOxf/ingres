test_that("computePbnByCluster results don't contain NAs", {
  ingres.object = createIngresObjectFromSeurat(pbmc_small, "RNA", "data",
                                        network_genes, network)
  ingres.object@viper = viperResults
  data = computePbnByCluster(ingres.object)@cluster.pbn
  expect_false(anyNA(data))
})

test_that("computePbnByCluster results are within the intended range", {
  ingres.object = createIngresObjectFromSeurat(pbmc_small, "RNA", "data",
                                        network_genes, network)
  ingres.object@viper = viperResults
  data = computePbnByCluster(ingres.object)@cluster.pbn %>%
    select(-n) %>%
    pivot_longer(!cluster, names_to = "var")

  data = data$value

  expect_true(is.numeric(data))
  expect_gte(min(data), -1000)
  expect_lte(max(data), 1000)
})


test_that("computePbnBySingleCell results don't contain NAs", {
  ingres.object = createIngresObjectFromSeurat(pbmc_small, "RNA", "data",
                                        network_genes, network)
  ingres.object@viper = viperResults

  data = computePbnBySingleCell(ingres.object)@single.cell.pbn
  expect_false(anyNA(data))
})


test_that("computePbnBySingleCell results are within the intended range", {
  ingres.object = createIngresObjectFromSeurat(pbmc_small, "RNA", "data",
                                        network_genes, network)
  ingres.object@viper = viperResults

  data = computePbnBySingleCell(ingres.object)@single.cell.pbn %>%
    pivot_longer(!c(cell, cluster), names_to = "var")

  data = data$value

  expect_true(is.numeric(data))
  expect_gte(min(data), -1000)
  expect_lte(max(data), 1000)
})
