
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ingres

<!-- badges: start -->

[![R-CMD-check-bioc](https://github.com/CBigOxf/ingres/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/CBigOxf/ingres/actions)
<!-- badges: end -->

***I**nferring Probabilistic Boolean **N**etworks of **G**ene
**R**egulation Using Protein Activity **E**nrichment **S**cores*

Given a gene regulatory boolean network and a RNA-seq dataset,
**ingres** computes protein activity normalised enrichment scores using
VIPER, and then produces a probabilistic network using the scores as
probabilities for fixed node activation or deactivation, in addition to
the original boolean functions.

## Installation

You can install the development version of **ingres** with:

``` r
# install.packages("devtools")
devtools::install_github("CBigOxf/ingres")
```
