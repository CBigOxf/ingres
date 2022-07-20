test_that("object is created from Seurat object", {
  obj = createIngresObjectFromSeurat(
    small_blca_wang,
    "RNA", "scale.data",
    network_genes,
    network
  )
  expect_s4_class(obj, "ingres")
})
