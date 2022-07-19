test_that("graphmlAsTidy creates a valid tidygraph object", {
  tidyg = graphmlAsTidy(test_path('example_network.graphml'))

  expect_s3_class(tidyg, 'tbl_graph')

  tb = tidyg %>% tidygraph::activate('nodes') %>% tidygraph::as_tibble()

  expect_true(nrow(tb)>0)

  expect_false(any(is.na(tb)))
})

test_that('produceBoolnetNetwork converts to Boolnet object correctly', {
  network = ingresAfterViper %>%
    computePbnByCluster() %>%
    produceNetworkForCluster('1')

  bn = produceBoolnetNetwork(network)

  expect_s3_class(bn, 'ProbabilisticBooleanNetwork')

  expect_false(any(is.na(bn)))

  expect_true(length(bn$fixed)>0 && length(bn$genes)>0)
})

test_that('createNetworkGenesTemplate correctly creates the template file', {
  createTest = function(){
    tempdir = tempdir()
    createNetworkGenesTemplate(network, dir = tempdir, store = T, modify = F)
    paste0(tempdir, "/networkGenes.csv")
  }

  announce_snapshot_file(name = "networkGenes.csv")
  expect_snapshot_file(createTest(), "networkGenes.csv", cran = T)
})

test_that('createNetworkGenesTemplate correctly returns the template file', {
  expect_snapshot(
    createNetworkGenesTemplate(network, store = F, modify = F),
    cran = T)
})

test_that('ginmlToGraphml correctly returns a graphml file as a vector', {
  graphml = ginmlToGraphml(test_path('example_ginsim.zginml'),
                           dest = withr::local_tempfile(fileext = '.graphml'))
  expect_snapshot(
    ginmlToGraphml(test_path('example_ginsim.zginml'), dest =
                     withr::local_tempfile(fileext = '.graphml')), cran = T)
})

test_that('ginmlToGraphml correctly creates a graphml file', {
  createGraphml = function(){
    path = tempfile(fileext = '.graphml')
    ginmlToGraphml(test_path('example_ginsim.zginml'),
                   dest = path)
    return(path)
  }
  announce_snapshot_file('snapshot.graphml')
  expect_snapshot_file(createGraphml(), name = 'snapshot.graphml', cran = T)
})

test_that('ginmlToGraphml recognises given fates', {
  networkFates = c('Proliferation', 'Apoptosis')
  path = withr::local_tempfile(fileext = '.graphml')
  graphml = ginmlToGraphml(test_path('example_ginsim.zginml'),
                           dest = path,
                           fates = networkFates)

  tn = graphmlAsTidy(path)
  fateNodes = tn %>% tidygraph::activate('nodes') %>%
    tidygraph::as_tibble() %>%
    filter(kind == 'fate')

  expect_true(nrow(fateNodes)==2)
  expect_true(fateNodes$id[1] %in% networkFates)
  expect_true(fateNodes$id[2] %in% networkFates)
})

test_that('printAllNodes correctly prints a network', {
  expect_snapshot(printAllNodes(network), cran = T)
})