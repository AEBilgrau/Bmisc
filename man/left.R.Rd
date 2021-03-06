% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left.R
\name{left}
\alias{left}
\alias{left.default}
\alias{left.matrix}
\alias{right}
\alias{right.default}
\alias{right.matrix}
\title{View left and right parts of objects}
\usage{
left(x, ...)

\method{left}{default}(x, n = 6L, ...)

\method{left}{matrix}(x, n = 6L, ...)

right(x, ...)

\method{right}{default}(x, n = 6L, ...)

\method{right}{matrix}(x, n = 6L, addcolnums = TRUE, ...)
}
\arguments{
\item{x}{The object.}

\item{\dots}{Arguments passed to other methods.}

\item{n}{A single \code{integer}. If positive, the \code{n} left/right-most
parts. If negative, it returns all but the \code{n} left/right-most 
parts.}

\item{addcolnums}{Logical. Should the column 'fake' numbers be added to the 
output as column names?
Default is \code{TRUE}.}
}
\value{
Usually returns an object of the same class as \code{x} but small and
  more compact.
}
\description{
Extension of the functionality of \code{head} and \code{tail} with functions
\code{left} and \code{right} and combinations hereof.
Like \code{head} and \code{tail} returns the top and bottom parts of an 
object, \code{left} and \code{right} return the left- and right-most parts of 
the object. The functions are primarily oriented toward classes 
\code{matrix}, \code{data.frame}, and other matrix-like objects classes but 
are generic and may be extended to other classes if sensical.
}
\details{
\code{top} and \code{bot} are simply
}
\examples{
x <- matrix(1:100, 10, 10)
head(x)
head(x, n = -2)
tail(x)
left(x)
left(x, n = -2L)
right(x)
right(x, n = -2)
right(tail(x))

right(x, addcolnums = TRUE)
right(x, addcolnums = FALSE)
}
\seealso{
Main documentation of \code{\link{head}} and \code{\link{tail}}
}
\author{
Modified code from \code{\link{head}} and \code{\link{tail}} and 
  the corresponding class-specific methods. \cr
  Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
}
