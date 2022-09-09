#' Compute a Probabilistic Boolean Network (PBN) for each cluster or each cell
#'
#' After running \code{\link{performViper}}, this can be run to produce a
#' PBN with the identity function probability for each node relative to the
#' VIPER normalised enrichment score (NES) for cell. For `computePbnByCluster`,
#' the median NES for all cells in that cluster will be used.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a
#' non-null VIPER slot.
#' @param range A vector representing the range to which the NES are
#' to be re-scaled. \code{range[1]+range[2] == 0} should be TRUE.
#' Defaults to \[-1, 1\]
#'
#' @return An \code{\linkS4class{ingres}} object with
#' the `cluster.pbn` or the `single.cell.pbn` slot filled
#'
#' @describeIn computePbnByCluster
#' Compute a Probabilistic Boolean Network (PBN) for each cluster
#'
#' @examples
#' # Create an ingres object with viper slot
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#'
#' ing = computePbnByCluster(ing)
#' head(ing@cluster.pbn)
#'
#' ing = computePbnBySingleCell(ing)
#' head(ing@single.cell.pbn)
#'
#' # Restrict range to (-0.5, 0.5)
#' ing = computePbnByCluster(ing, range = c(-0.5, 0.5))
#' head(ing@cluster.pbn)
#'
#' ing = computePbnBySingleCell(ing, range = c(-0.5, 0.5))
#' head(ing@single.cell.pbn)
#'
#' @export
computePbnByCluster = function(ingres.object, range = c(-1, 1)) {
  checkRange(range)
  viper.result = suppressMessages(tibble(ingres.object@viper,
    .name_repair = "unique"
  )) # ensure unique, non empty column names
  counts = viper.result %>%
    select(c(1, 2)) %>%
    count(.data$cluster)
  result = viper.result %>%
    select(-1) %>%
    group_by(.data$cluster) %>%
    summarise(across(
      .cols = tidyselect::everything(),
      stats::median
    )) %>%
    pivot_longer(!.data$cluster, names_to = "symbol") %>%
    pivot_wider(names_from = .data$cluster, values_from = .data$value) %>%
    merge(ingres.object@network.genes) %>%
    select(-.data$symbol) %>%
    group_by(.data$node) %>%
    summarise(across(.cols = tidyselect::everything(), mean)) %>%
    mutate(across(
      !.data$node, ~ scales::rescale_mid(.x, to = range, mid = 0)
    )) %>%
    pivot_longer(!.data$node, names_to = "cluster") %>%
    pivot_wider(names_from = .data$node, values_from = .data$value) %>%
    merge(counts) %>%
    relocate(n, .after = .data$cluster)
  ingres.object@cluster.pbn = result
  ingres.object
}

#' @describeIn computePbnByCluster
#' Compute a Probabilistic Boolean Network (PBN) for each cell
#' @export
computePbnBySingleCell = function(ingres.object, range = c(-1, 1)) {
  checkRange(range)
  viper.result = suppressMessages(tibble(ingres.object@viper,
    .name_repair = "unique"
  )) # ensure unique, non empty column names
  identities = viper.result %>% select(c(1, 2))
  result = viper.result %>%
    select(-2) %>%
    pivot_longer(!.data$cell, names_to = "symbol") %>%
    pivot_wider(names_from = .data$cell, values_from = .data$value) %>%
    merge(ingres.object@network.genes) %>%
    select(-.data$symbol) %>%
    group_by(.data$node) %>%
    summarise(across(.cols = tidyselect::everything(), mean)) %>%
    mutate(across(
      !.data$node, ~ scales::rescale_mid(.x, to = range, mid = 0)
    )) %>%
    pivot_longer(!.data$node, names_to = "cell") %>%
    pivot_wider(names_from = .data$node, values_from = .data$value) %>%
    merge(identities) %>%
    relocate(.data$cluster, .after = .data$cell)
  ingres.object@single.cell.pbn = result
  ingres.object
}

checkRange = function(range) {
  if (!is.numeric(range)) {
    stop("'range' must be a numeric vector")
  }
  if (length(range) != 2) {
    stop("Length of 'range' must be 2")
  }

  if ((range[1] + range[2]) != 0) {
    stop("The sum of the elements of 'range' must be equal to 0")
  }

  if ((range[1] > range[2])) {
    stop("The second element of 'range' should be greater than the first")
  }
}
