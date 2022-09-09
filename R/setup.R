.onLoad = function(libname, pkgname) {
  biocPkgs = c("viper", "AnnotationDbi", "org.Hs.eg.db")
  for (pkg in biocPkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste(
        "Package '", pkg,
        "' could not be loaded. \n
                     Please install it from Bioconductor\n
                     install.packages(\"BiocManager\")\n
                     BiocManager::install(", pkg, ")"
      ),
      call. = FALSE
      )
    }
  }
}
