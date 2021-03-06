% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/head.list.R
\name{head.list}
\alias{head.list}
\alias{tail.list}
\title{Return the first or last part of a list}
\usage{
\method{head}{list}(x, n = 6L, m = n, ...)

\method{tail}{list}(x, n = 6L, m = n, ...)
}
\arguments{
\item{x}{A list of R objects.}

\item{n}{A single integer. If positive, size for the resulting object:
number of elements for a vector (including lists), rows for a matrix or
data frame or lines for a function. If negative, all but the n last/first
number of elements of x.}

\item{m}{As \code{n}. If \code{x} is a list, \code{m} controls the number of
elements from the list to show. Defaults to \code{n}.}

\item{\dots}{Arguments passed to \code{head}.}
}
\value{
Returns a list of length \code{m} of the first/last element of
  \code{x} where the first \code{n} elements of each entry is shown.
}
\description{
This function recurses over the first/last elements of a list to return a
compact view of the list.
}
\examples{
x <- list(A = letters[1:9],           B = matrix(runif(30), ncol = 3),
          C = as.list(LETTERS[1:10]), D = function(x) { x^2 + 2*x + 1},
          D = combn(1:5, m = 4),      E = "A simple string.",
          F = data.frame("unif" = runif(10), "norm" = rnorm(10)))
str(x)
head(x, n = 3L)
head(x, n = 2L, m = 7L)
tail(x, n = 3L)
tail(x, n = 3L, m = 3L)
}
\seealso{
\code{\link[utils]{head}}
}
