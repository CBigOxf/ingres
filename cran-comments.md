-   This is a new release.

## R CMD check results

0 errors \| 0 warnings \| 2 note

I ran R CMD checks on Windows and Linux and there were no errors, no warnings and two notes. In MacOS the check may fail if the user doesn't install the package 'rgdal' manually first, this is a dependency of Seurat, which is suggested by ingres, not a hard dependency.

The notes are:

> \* checking CRAN incoming feasibility ... NOTE Maintainer: 'Pedro Victori [pedroluisvictori\@gmail.com](mailto:pedroluisvictori@gmail.com){.email}'
>
> New submission
>
> Package has a FOSS license but eventually depends on the following package which may restrict use: viper
>
> Found the following (possibly) invalid URLs: URL: <https://codecov.io/gh/CBigOxf/ingres> (moved to <https://app.codecov.io/gh/CBigOxf/ingres>) From: README.md Status: 200 Message: OK

I can't do anything about viper's license, as there is not an appropriate FOSS substitute.

I have tested the URL and it is valid.

> checking package dependencies ... NOTE Package suggested but not available for checking: 'aracne.networks'

The package "aracne.networks" seems like it does not install in automatic checks, but it is available from BioConductor. Nevertheless it is just a suggested package and I have guarded its use in examples and vignette.

## Preprint

The in-depth explanation of the purpose and implementation of this package can be found in this pre-print: <https://www.biorxiv.org/content/10.1101/2022.09.04.506528v1> and also in the accompanying vignette.

## Dependencies from BioConductor

The user is instructed in the README to install the BioConductor dependencies, like so:

> You may need to manually install some of the BioConductor dependencies before you can install ingres:

> ``` r
> install.packages("BiocManager")
> BiocManager::install(c("viper", "AnnotationDbi", "org.Hs.eg.db", "aracne.networks"))
> ```

## 
