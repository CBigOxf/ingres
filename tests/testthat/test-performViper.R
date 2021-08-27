test_that("performViper finish without errors", {
  obj = createExampleIngresObject()

  expect_error(
    performViper(obj, regulon = aracne.networks::regulonbrca), NA)
})

test_that("performViper returns the correct messages and output", {
  obj = createExampleIngresObject()

  expect_snapshot(
    performViper(obj, regulon = aracne.networks::regulonbrca),
    cran = TRUE)
})
