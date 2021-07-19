test_that("performViper finish without errors", {
  obj = createIngresObjectFromSeurat(pbmc_small,
                                     "RNA", "data",
                                     network_genes,
                                     network)

  expect_error(
    performViper(obj, regulon = aracne.networks::regulonbrca), NA)
})

test_that("performViper returns the correct messages and output", {
  obj = createIngresObjectFromSeurat(pbmc_small,
                                     "RNA", "data",
                                     network_genes,
                                     network)

  expect_snapshot(
    performViper(obj, regulon = aracne.networks::regulonbrca),
    cran = TRUE)
})
