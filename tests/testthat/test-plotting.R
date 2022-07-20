test_that("The plots produced match the snapshots", {
  obj = ingresAfterViper
  obj %<>% computePbnByCluster() %>% computePbnBySingleCell()
  cell.id = "sample1@ACAGCTAAGATCCCGC-1"
  cluster.id = "1"
  expect_snapshot_plot(
    "cellPbnPlotSnap",
    obj %>% cellPbnPlot(cell.id), 10, 15
  )
  expect_snapshot_plot(
    "clusterPbnPlotSnap",
    obj %>% clusterPbnPlot(cluster.id), 10, 15
  )
  expect_snapshot_plot(
    "clusterGenesHeatmapSnap",
    obj %>% clusterGenesHeatmap(), 10, 15
  )
  expect_snapshot_plot(
    "cellGenesHeatmapSnap",
    obj %>% cellGenesHeatmap(), 10, 15
  )
})
