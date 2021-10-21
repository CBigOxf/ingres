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

test_that("The plots produced match the snapshots", {
  obj = createExampleIngresObjectWithViper()
  obj %<>% computePbnByCluster() %>% computePbnBySingleCell()
  cell.id = 'AAACCTGCACGACGAA'
  cluster.id = '3-Proliferation'
  expect_snapshot_plot('cellPbnPlotSnap', obj %>% cellPbnPlot(cell.id), 10, 15)
  expect_snapshot_plot('clusterPbnPlotSnap', obj %>% clusterPbnPlot(cluster.id), 10, 15)
  expect_snapshot_plot('clusterGenesHeatmapSnap', obj %>% clusterGenesHeatmap(), 10, 15)
  expect_snapshot_plot('cellGenesHeatmapSnap', obj %>% cellGenesHeatmap(), 10, 15)
})


