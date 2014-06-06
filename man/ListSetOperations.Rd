\name{SetOperations}
\alias{Union}
\alias{Intersect}
\alias{Setdiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set operations on lists of character vectors
}
\description{
These function perform basic set operations on lists of character vectors in a recursive manner. The work analogous to \code{union}, \code{intersect}, and \code{setdiff} but on lists.
}
\usage{
Union(x)
Intersect(x)
Setdiff(x, y)
}
\arguments{
  \item{x}{
    A \code{list} of \code{character} vectors.
  }
  \item{y}{
    A \code{list} of \code{character} vectors.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
All functions return a single character vector.
\code{Union} returns the union all character vectors in \code{x}.
\code{Intersect} returns the intersections of all character vectors in \code{x}.
\code{Setdiff} returns the intersection of \code{x} minus the union of \code{y}. 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau \email{abilgrau@math.aau.dk}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{union}}, \code{\link{intersect}}, \code{\link{setdiff}}
}
\examples{
set.seed(1)
x <- lapply(2:6, function(i) sample(LETTERS, 10+i))
y <- lapply(1:3, function(i) sample(LETTERS, 2+i))

Union(x)  # The union of all vectors in x
Intersect(x)  # The intersection of all vectors in x
Setdiff(x, y)  # All elements in the intersection of x, but not in any y
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ union }
\keyword{ intersection }% __ONLY ONE__ keyword per line
\keyword{ set difference }
