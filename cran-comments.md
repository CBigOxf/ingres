-   This is a new release.

## R CMD check results

0 errors \| 0 warnings \| 0 note

I ran R CMD checks on Windows and Linux and there were no errors, no warnings and no notes. In MacOS the check may fail if the user doesn't install the package 'rgdal' manually first, this is a dependency of Seurat, which is suggested by ingres, not a hard dependency.

## Dependencies from BioConductor

The user is instructed in the README to install the BioConductor dependencies, like so:

> You may need to manually install some of the BioConductor dependencies before you can install ingres:
>
> ``` r
> install.packages("BiocManager")
> BiocManager::install(c("viper", "AnnotationDbi", "org.Hs.eg.db", "aracne.networks"))
> ```
