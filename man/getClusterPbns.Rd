% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingresClass.R
\name{getClusterPbns}
\alias{getClusterPbns}
\title{Get the cluster PBNs from an \code{\linkS4class{ingres}} object}
\usage{
getClusterPbns(ingres.object)
}
\arguments{
\item{ingres.object}{an \code{\linkS4class{ingres}} object}
}
\value{
A data frame containing the generated cluster PBNs
}
\description{
Get the cluster PBNs from an \code{\linkS4class{ingres}} object
}
\examples{
ing <- createExampleIngresObjectWithViper()
ing <- ing \%>\% computePbnByCluster()
head(getClusterPbns(ing))
}
