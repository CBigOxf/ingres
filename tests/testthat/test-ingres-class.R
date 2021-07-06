test_that("object is created from Seurat object", {
  obj = createIngresObjectFromSeurat(pbmc_small,
                                     "RNA", "scale.data",
                                     network_genes,
                                     network)
  expect_s4_class(obj, "ingres")
})
