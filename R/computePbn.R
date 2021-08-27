#' Compute a Probabilistic Boolean Network (PBN) for each cluster.
#'
#' After running \code{\link{performViper}}, this can be run to produce a PBN with
#' the identity function probability for each node relative to the median VIPER
#' normalised enrichment score (NES) for each cluster.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null VIPER slot.
#' @param range A vector representing the range to which the NES are to be calculated.
#'  \code{range[1]+range[2] == 0 & range[2] <= 1} should be TRUE. Defaults to \[-1, 1\]
#'
#' @return An \code{\linkS4class{ingres}} object with the \code{cluster.pbn slot} filled
#' @export
computePbnByCluster = function(ingres.object, range = c(-1, 1)){
  checkRange(range)
  viper.result = tibble(ingres.object@viper,
                        .name_repair = "unique") #ensure unique, non empty column names
  counts = viper.result %>% select(c(1,2)) %>% count(cluster)
  result = viper.result %>%
    select(-1) %>%
    group_by(cluster) %>%
    summarise(across(.cols = tidyselect::everything(),
                     stats::median)) %>% #why median here but mean below?
    pivot_longer(!cluster, names_to = "symbol") %>%
    pivot_wider(names_from = cluster, values_from = value) %>%
    merge(ingres.object@network.genes) %>%
    select(-symbol) %>%
    group_by(node) %>%
    summarise(across(.cols = tidyselect::everything(), mean)) %>%
    mutate(across(!node, ~ scales::rescale_mid(.x, to = range, mid = 0))) %>%
    pivot_longer(!node, names_to = "cluster") %>%
    pivot_wider(names_from = node, values_from = value) %>%
    merge(counts) %>%
    relocate(n, .after = cluster)
  ingres.object@cluster.pbn = result
  ingres.object
}

#' Compute a Probabilistic Boolean Network (PBN) for each cell.
#'
#' After running \code{\link{performViper}}, this can be run to produce a PBN with
#' the identity function probability for each node relative to the VIPER
#' normalised enrichment score (NES) for every cell.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null VIPER slot.
#' @param range A vector representing the range to which the NES are to be calculated.
#'  \code{range[1]+range[2] == 0 & range[2] <= 1} should be TRUE. Defaults to \[-1, 1\].
#' @return An \code{\linkS4class{ingres}} object with the \code{single.cell.pbn slot} filled
#' @export
computePbnBySingleCell = function(ingres.object, range = c(-1, 1)){
  checkRange(range)
  viper.result = tibble(ingres.object@viper,
                        .name_repair = "unique") #ensure unique, non empty column names
  identities = viper.result %>% select(c(1,2))
  result = viper.result %>%
    select(-2) %>%
    pivot_longer(!cell, names_to = "symbol") %>%
    pivot_wider(names_from = cell, values_from = value) %>%
    merge(ingres.object@network.genes) %>%
    select(-symbol) %>%
    group_by(node) %>%
    summarise(across(.cols = tidyselect::everything(), mean)) %>%
    mutate(across(!node, ~ scales::rescale_mid(.x, to = range, mid = 0))) %>%
    pivot_longer(!node, names_to = "cell") %>%
    pivot_wider(names_from = node, values_from = value) %>%
    merge(identities) %>%
    relocate(cluster, .after = cell)
  ingres.object@single.cell.pbn = result
  ingres.object
}

checkRange = function(range){
  if(!is.numeric(range)){
    stop("'range' must be a numeric vector")
  }
  if(length(range) != 2){
    stop("Length of 'range' must be 2")
  }

  if((range[1] + range[2]) != 0){
    stop("The sum of the elements of 'range' must be equal to 0")
  }

  if((range[1] > range[2])){
    stop("The second element of 'range' should be greater than the first")
  }

  if((range[2] > 1)){
    stop("'range' should be inside [-1, 1]")
  }
}
