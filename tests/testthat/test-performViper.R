test_that("performViper returns the correct messages and output", {
  obj = createExampleIngresObject()
  expect_snapshot(
    performViper(obj, regulon = aracne.networks::regulonbrca)@viper,
    cran = TRUE)
})
