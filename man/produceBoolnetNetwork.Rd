% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netwokHelpers.R
\name{produceBoolnetNetwork}
\alias{produceBoolnetNetwork}
\title{Convert an ingres network into a BoolNet one}
\usage{
produceBoolnetNetwork(network)
}
\arguments{
\item{network}{The network to be converted, created
by \code{\link[=produceNetworkForCluster]{produceNetworkForCluster()}} or \code{\link[=produceNetworkForCell]{produceNetworkForCell()}}}
}
\value{
A BoolNet object.
}
\description{
Convert an ingres network into a BoolNet one
}
\examples{
# Create an ingres object with viper slot
ing <- createIngresObjectFromSeurat(
  small_blca_wang, "RNA", "data", network_genes, network
)
ing@viper <- viper_results

# Compute PBNs by cluster
ing <- computePbnByCluster(ing)

# Produce a network for an arbitrary cluster
network <- produceNetworkForCluster(ing, "1")

produceBoolnetNetwork(network)

}
