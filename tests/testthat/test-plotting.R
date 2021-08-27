test_that("no errors are thrown from plotting functions", {
  obj = createExampleIngresObjectWithViper()
  obj %<>% computePbnByCluster() %>% computePbnBySingleCell()
  cell.id = 'AAACCTGCACGACGAA'
  cluster.id = '3-Proliferation'
  expect_error(obj %>% cellPbnPlot(cell.id), NA)
  expect_error(obj %>% clusterPbnPlot(cluster.id), NA)
  expect_error(obj %>% clusterGenesHeatmap(), NA)
  expect_error(obj %>% cellGenesHeatmap(), NA)
})
