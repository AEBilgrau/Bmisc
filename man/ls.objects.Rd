% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ls.objects.R
\name{.ls.objects}
\alias{.ls.objects}
\alias{ls.objects}
\alias{lsos}
\title{List objects and their memory usage}
\usage{
.ls.objects(pos = 1L, pattern, order.by, decreasing = FALSE,
  head = FALSE, n = 5)

lsos(..., n = 10)
}
\arguments{
\item{pos}{Specifies the environment as a position in the search list.}

\item{pattern}{an optional regular expression. Only names matching pattern
are returned.}

\item{order.by}{A character giving the column-name to sort by. Can be one of
\code{"Type"}, \code{"Size"}, \code{"Rows"}, or
\code{"Columns"}.
If not supplied, the \code{data.frame} is not ordered.}

\item{decreasing}{logical. Should the rows be in decreasing order?}

\item{head}{logical. Should only the rows be returned?}

\item{n}{An integer giving the number of rows to be printed. Only used in
\code{head} is \code{TRUE}.}

\item{\dots}{Parameters assed to \code{ls.objects}.}
}
\value{
A \code{data.frame} with columns \code{"Type"}, \code{"Size"},
  \code{"Rows"}, \code{"PrettySize"}, and \code{"Columns"}.
}
\description{
This functions lists the object names, type, size, and number of rows and
columns in the specificed enviroment. \code{lsos()} is a shorthand interface
to list the most memory heavy objects.
}
\examples{
A <- 1
B <- cbind(1:2, 2:3)
C <- replicate(10, rnorm(1e6))
Bmisc:::.ls.objects()
Bmisc:::.ls.objects(order.by = "Size")
lsos()
}
\references{
From
  \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
}
\author{
Based on Dirk Eddelbuettel, Petr Pikal, David Hinds, Tony Breyal,
  JD Long
}
