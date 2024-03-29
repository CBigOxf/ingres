% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingresClass.R
\name{createIngresObject}
\alias{createIngresObject}
\title{Create a new \code{ingres} object.}
\usage{
createIngresObject(expression.matrix, idents, network.genes, network)
}
\arguments{
\item{expression.matrix}{A single-cell expression matrix,
with cells in rows and genes in columns.}

\item{idents}{A data frame with a column for cell barcode
and another column for the corresponding cluster or subpopulation.}

\item{network.genes}{A data frame with a column for node names
and another column for the corresponding entrez IDs.}

\item{network}{A object of class \code{tbl_graph} representing
our network of interest.}
}
\value{
An \code{ingres} object.
}
\description{
Create a new \code{ingres} object.
}
\examples{
# Get expression matrix from small_blca_wang for convenience, but it can be
# any single-cell expression matrix, from any source. Same for idents.
expression.matrix <-
  as.matrix(Seurat::GetAssayData(
    small_blca_wang,
    assay = "RNA", slot = "data"
  ))

idents <- data.frame(
  cell = names(Seurat::Idents(small_blca_wang)),
  cluster = as.character(Seurat::Idents(small_blca_wang)),
  check.names = FALSE
)

ing <- createIngresObject(expression.matrix, idents, network_genes, network)
ing

}
