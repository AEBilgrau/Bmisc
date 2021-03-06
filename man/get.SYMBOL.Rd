% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.SYMBOL.R
\name{get.SYMBOL}
\alias{get.SYMBOL}
\title{Get Hugo gene symbols and ensembl gene identifiers}
\usage{
get.SYMBOL(es, subset = featureNames(es), attributes = NULL, ...)
}
\arguments{
\item{es}{in an ExpressionSet object}

\item{subset}{is a character vector of feature names.}

\item{attributes}{other attributes to extract. A possible list of attributes
can be retrieved using the function \code{listAttributes}.}

\item{\dots}{arguments passed to \code{getBM}.}
}
\value{
a \code{data.frame} with columns corresponding to the matched gene identifiers.
}
\description{
These functions uses biomaRt to get gene symbols and ensembl gene identifies 
for an ExpressionSet object.
}
\author{
Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
}
