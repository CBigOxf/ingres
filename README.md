
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ingres

<!-- badges: start -->

[![R-CMD-check](https://github.com/CBigOxf/ingres/workflows/R-CMD-check/badge.svg)](https://github.com/CBigOxf/ingres/actions)[![codecov](https://codecov.io/gh/CBigOxf/ingres/branch/master/graph/badge.svg?token=XQHKMZPKEB)](https://app.codecov.io/gh/CBigOxf/ingres)
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

You may need to manually install some of the BioConductor dependencies
before you can install **ingres**:

``` r
# install.packages("BiocManager")
BiocManager::install(c("viper", "AnnotationDbi", "org.Hs.eg.db", "aracne.networks"))
```

## Citation

To cite **ingres** in publications use:

*Victori, P. & Buffa, F. M. Ingres: from single-cell RNA-seq data to
single-cell probabilistic Boolean networks. 2022.09.04.506528 Preprint
at <https://doi.org/10.1101/2022.09.04.506528> (2022).*
