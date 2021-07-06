test_that("computePbn functions work", {
  obj = createIngresObjectFromSeurat(pbmc_small,
                                     "RNA", "scale.data",
                                     network_genes,
                                     network)
  obj@viper = viperResults

  numericTests <- function(data) {
    expect_true(is.numeric(data))
    min = data %>% min()
    max = data %>% max()
    expect_gte(min, -1000)
    expect_lte(max, 1000)
  }

  obj %<>% computePbnByCluster() %>% computePbnBySingleCell()
  result.cluster = obj@cluster.pbn
  result.sc = obj@single.cell.pbn

  expect_false(anyNA(result.cluster))
  expect_false(anyNA(result.sc))

  pivoted_df.cluster = result.cluster %>%
    select(-n) %>%
    pivot_longer(!cluster, names_to = "var")
  numericTests(pivoted_df.cluster$value)

  pivoted_df.sc = result.sc %>%
    pivot_longer(!c(cell, cluster), names_to = "var")
  numericTests(pivoted_df.sc$value)
})
