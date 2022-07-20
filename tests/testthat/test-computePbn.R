test_that("computePbnByCluster results don't contain NAs", {
  ingres.object = ingresAfterViper
  data = computePbnByCluster(ingres.object)@cluster.pbn
  expect_false(anyNA(data))
})

test_that("computePbnByCluster results are not empty", {
  ingres.object = ingresAfterViper
  data = computePbnByCluster(ingres.object)@cluster.pbn
  expect_true(nrow(data) > 0)
})

test_that("computePbnByCluster results are within the intended range", {
  ingres.object = ingresAfterViper

  testRange = function(data, min, max) {
    data %<>% select(-n) %>%
      pivot_longer(!cluster, names_to = "var")

    data = data$value

    expect_true(is.numeric(data))
    expect_gte(min(data), min)
    expect_lte(max(data), max)
  }

  test = computePbnByCluster(ingres.object)@cluster.pbn
  testRange(test, -1, 1)

  test = computePbnByCluster(ingres.object, c(-0.5, 0.5))@cluster.pbn
  testRange(test, -0.5, 0.5)

  test = computePbnByCluster(ingres.object, c(-0.01, 0.01))@cluster.pbn
  testRange(test, -0.01, 0.01)
})


test_that("computePbnBySingleCell results don't contain NAs", {
  ingres.object = ingresAfterViper
  data = computePbnBySingleCell(ingres.object)@single.cell.pbn
  expect_false(anyNA(data))
})

test_that("computePbnBySingleCell results are not empty", {
  ingres.object = ingresAfterViper
  data = computePbnBySingleCell(ingres.object)@single.cell.pbn
  expect_true(nrow(data) > 0)
})

test_that("computePbnBySingleCell results are within the intended range", {
  ingres.object = ingresAfterViper

  testRange = function(data, min, max) {
    data %<>% pivot_longer(!c(cell, cluster), names_to = "var")

    data = data$value

    expect_true(is.numeric(data))
    expect_gte(min(data), min)
    expect_lte(max(data), max)
  }

  test = computePbnBySingleCell(ingres.object)@single.cell.pbn
  testRange(test, -1, 1)

  test = computePbnBySingleCell(ingres.object, c(-0.5, 0.5))@single.cell.pbn
  testRange(test, -0.5, 0.5)

  test = computePbnBySingleCell(ingres.object, c(-0.01, 0.01))@single.cell.pbn
  testRange(test, -0.01, 0.01)
})

test_that("computePbnByCluster throws the corresponding errors with wrong ranges", {
  ingres.object = ingresAfterViper

  expect_error(ingres.object %>% computePbnByCluster(), NA)
  expect_error(ingres.object %>% computePbnByCluster(c(-0.5, 0.5)), NA)

  expect_error(
    ingres.object %>% computePbnByCluster("1, 1"),
    "'range' must be a numeric vector"
  )

  expect_error(
    ingres.object %>% computePbnByCluster(c(-1, 0, 1)),
    "Length of 'range' must be 2"
  )

  expect_error(
    ingres.object %>% computePbnByCluster(c(-0.5, 1)),
    "The sum of the elements of 'range' must be equal to 0"
  )

  expect_error(
    ingres.object %>% computePbnByCluster(c(1, -1)),
    "The second element of 'range' should be greater than the first"
  )
})

test_that("computePbnBySingleCell throws the corresponding errors with wrong ranges", {
  ingres.object = ingresAfterViper

  expect_error(ingres.object %>% computePbnBySingleCell(), NA)
  expect_error(ingres.object %>% computePbnBySingleCell(c(-0.5, 0.5)), NA)

  expect_error(
    ingres.object %>% computePbnBySingleCell("1, 1"),
    "'range' must be a numeric vector"
  )

  expect_error(
    ingres.object %>% computePbnBySingleCell(c(-1, 0, 1)),
    "Length of 'range' must be 2"
  )

  expect_error(
    ingres.object %>% computePbnBySingleCell(c(-0.5, 1)),
    "The sum of the elements of 'range' must be equal to 0"
  )

  expect_error(
    ingres.object %>% computePbnBySingleCell(c(1, -1)),
    "The second element of 'range' should be greater than the first"
  )
})
