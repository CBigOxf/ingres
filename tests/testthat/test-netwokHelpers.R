test_that("graphmlAsTidy creates a valid tidygraph object", {
  tidyg = graphmlAsTidy(
    system.file("extdata", "example_network.graphml", package = "ingres")
  )

  expect_s3_class(tidyg, "tbl_graph")

  tb = tidyg %>%
    tidygraph::activate("nodes") %>%
    tidygraph::as_tibble()

  expect_true(nrow(tb) > 0)

  expect_false(any(is.na(tb)))
})

test_that("produceBoolnetNetwork converts to Boolnet object correctly", {
  network = ingresAfterViper %>%
    computePbnByCluster() %>%
    produceNetworkForCluster("1")

  bn = produceBoolnetNetwork(network)

  expect_s3_class(bn, "ProbabilisticBooleanNetwork")

  expect_false(any(is.na(bn)))

  expect_true(length(bn$fixed) > 0 && length(bn$genes) > 0)
})

test_that("createNetworkGenesTemplate correctly creates the template file", {
  createTest = function() {
    tempdir = tempdir()
    createNetworkGenesTemplate(network, dir = tempdir, store = T, modify = F)
    paste0(tempdir, "/networkGenes.csv")
  }

  announce_snapshot_file(name = "networkGenes.csv")
  skip_on_cran()
  skip_on_ci()

  expect_snapshot_file(createTest(), "networkGenes.csv", cran = F)
})

test_that("createNetworkGenesTemplate correctly returns the template file", {
  expect_snapshot(
    createNetworkGenesTemplate(network, store = F, modify = F),
    cran = T
  )
})

test_that("ginmlToGraphml correctly returns a graphml file as a vector", {
  expect_snapshot(
    ginmlToGraphml(
      system.file("extdata", "example_ginsim.zginml", package = "ingres"),
      dest =
        withr::local_tempfile(fileext = ".graphml")
    ),
    cran = T
  )
})

test_that("ginmlToGraphml correctly creates a graphml file", {
  createGraphml = function() {
    path = tempfile(fileext = ".graphml")
    ginmlToGraphml(
      system.file("extdata", "example_ginsim.zginml", package = "ingres"),
      dest = path
    )
    return(path)
  }
  announce_snapshot_file("snapshot.graphml")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(createGraphml(), name = "snapshot.graphml", cran = F)
})

test_that("ginmlToGraphml recognises given fates", {
  networkFates = c("Proliferation", "Apoptosis")
  path = withr::local_tempfile(fileext = ".graphml")
  graphml = ginmlToGraphml(
    system.file("extdata", "example_ginsim.zginml", package = "ingres"),
    dest = path,
    fates = networkFates
  )

  tn = graphmlAsTidy(path)
  fateNodes = tn %>%
    tidygraph::activate("nodes") %>%
    tidygraph::as_tibble() %>%
    filter(kind == "fate")

  expect_true(nrow(fateNodes) == 2)
  expect_true(fateNodes$id[1] %in% networkFates)
  expect_true(fateNodes$id[2] %in% networkFates)
})

test_that("printAllNodes correctly prints a network", {
  expect_snapshot(printAllNodes(network), cran = T)
})
