\name{alp}
\alias{alp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add transparency to colours
}
\description{
The function adds transparency to a character of colours names or hexadecimal values.
}
\usage{
alp(x, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character of colours. Either as names (e.g. \code{"blue"}) or hexadecimal (\code{"#00FF00"}).
}
  \item{alpha}{
A numeric value with the alpha level. If \code{alpha = 0} is completely transparent and  \code{alpha = 1} is opaque.
}
}
\details{
\code{NA}s are currently returned as white. May change in the future.
}
\value{
A character of hexadecimal values with alpha value added.
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
\code{\link{rgb}}
}
\examples{
cols <- c("Blue", "#FF0004", NA, "Tomato", "#00FFFF40")
alp(cols, alpha = 0.5)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
