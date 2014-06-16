\name{rowSds}
\alias{colSds}
\alias{colSds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute row and column standard deviations
}
\description{
The \code{rowSds} and \code{colSds} respectively computes the standard deviations of the rows and columns of the given matrix.
}
\usage{
rowSds(x)
colSds(x)
}
\arguments{
  \item{x}{
  A numeric matrix of size \code{n} times \code{m}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{rowSds} returns a numeric vector of length \code{n}.

\code{colSds} returns a numeric vector of length \code{m}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau <abilgrau@math.aau.dk>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rowMeans}}, \code{\link{colMeans}}
}
\examples{
X <- matrix(rnorm(50), 10, 5)

rowSds(X)
colSds(X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
