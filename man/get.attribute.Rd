% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.attribute.R
\name{get.attribute}
\alias{get.attribute}
\title{Convert from bioconductor database array name to biomaRt's array names}
\usage{
get.attribute(es)
}
\arguments{
\item{es}{is an ExpressionSet object}
}
\value{
A character of length \code{1} with the biomaRt array name
}
\description{
Convert from bioconductor database array name to biomaRt's array names
}
\note{
Note, not all arrays are implemented.
}
\seealso{
\code{\link[biomaRt]{useMart}}, \code{\link[biomaRt]{listMarts}}, 
  \code{\link[biomaRt]{useDataset}}, \code{\link[biomaRt]{listDatasets}}
}
\author{
Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
}
