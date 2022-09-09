test_that("performViper returns the correct messages and output", {
  skip_on_ci()
  skip_on_cran()
  obj = createExampleIngresObject()
  expect_snapshot(
    performViper(obj,
      regulon = aracne.networks::regulonbrca,
      verbose = T
    )@viper,
    cran = FALSE
  )
})
