% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMatrix.R
\name{plotMatrix}
\alias{plotMatrix}
\title{Plot matrix as heat map}
\usage{
plotMatrix(x, ...)
}
\arguments{
\item{x}{A \code{matrix} to plot.}

\item{\dots}{Arguments passed to \code{\link{image}}.}
}
\value{
Invisibly returns the output of \code{image}.
}
\description{
Plots a \code{matrix} as a heat map/image with the correct positions. Simply
a wrapper for \code{\link{image}}. It flips and transposes the matrix
in image it as it is printed. Good for visualizing large matrices.
}
\examples{
y <- matrix(rnorm(200), 20, 10)
plotMatrix(y)

z <- matrix(seq_len(200), 20, 10)
plotMatrix(z)
}
\seealso{
\code{\link{image}}
}
\author{
Anders Ellern Bilgrau
}
