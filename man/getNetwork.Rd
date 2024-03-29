% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingresClass.R
\name{getNetwork}
\alias{getNetwork}
\title{Get the network from an \code{\linkS4class{ingres}} object}
\usage{
getNetwork(ingres.object)
}
\arguments{
\item{ingres.object}{an \code{\linkS4class{ingres}} object}
}
\value{
The network as a object of class \code{tbl_graph}
}
\description{
Get the network from an \code{\linkS4class{ingres}} object
}
\examples{
ing <- createExampleIngresObject()
head(getNetwork(ing))
}
