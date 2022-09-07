.onLoad = function(libname, pkgname){
  biocPkgs = c("viper", "AnnotationDbi", "org.Hs.eg.db")
  for (pkg in biocPkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (interactive()) {
        base::cat("Package '", pkg, "' is not available or could not be loaded. Will now try to install it from Bioconductor (requires a working internet connection):\n")

        BiocManager::install(pkg)

        # Assert that the package can be successfully loaded
        if (!requireNamespace(pkg, quietly = TRUE)) {
          throw(paste("Package '", package, "' could not be loaded. Please install it from Bioconductor, cf. https://www.bioconductor.org/"))
        }
      } else {
        warning("Package '", package, "' could not be loaded. Without it, ingres will not work. Please install it from Bioconductor, cf. https://www.bioconductor.org/")
      }
    }
  }
}
