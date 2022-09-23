test_that("object is created from Seurat object", {
  obj <- createIngresObjectFromSeurat(
    ingres::small_blca_wang,
    "RNA", "scale.data",
    ingres::network_genes,
    ingres::network
  )
  expect_s4_class(obj, "ingres")
})
