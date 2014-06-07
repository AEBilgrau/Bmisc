\name{without}
\alias{without}
\alias{\%w/o\%}
\title{
The relative complement or set difference
}
\description{
The relative complement of x with respect to y, i.e. x\\y or x minus y.
}
\usage{
without(x, y)
x \%w/o\% y
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character or numeric vector.
}
  \item{y}{
A character or numeric vector.
}
}
\value{
The function returns a vector of the elements in x which does not appear in y.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Steffen Falgreen, Anders Ellern Bilgrau <abilgrau@math.aau.dk>
}
\note{
These functions are identical to \code{\link{setdiff}} but sometimes more easy to use.
}
\seealso{
\code{\link[base]{setdiff}}, \link[Bmisc]{ListSetOperations}
}
\examples{
print(x <- LETTERS[1:10])
print(y <- LETTERS[5:15])

x \%w/o\% y
without(x, y)
setdiff(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ setdiff }
\keyword{ setminus }
\keyword{ complement }% __ONLY ONE__ keyword per line
