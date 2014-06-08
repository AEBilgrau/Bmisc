\name{get.SYMBOL}
\alias{get.SYMBOL}
\alias{get.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get hugo and esembl gene symbols from biomaRt
}
\description{
These functions uses biomaRt to get gene symbols and ensembl gene identifies for an ExpressionSet object.
}
\usage{
get.SYMBOL(es, subset = biomaRt::featureNames(es), attributes = NULL)
get.attribute(es)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{es}{
An ExpressionSet object.
}
\item{subset}{
The features on the array to look up. Default is to use all features on the array.
}
\item{attributes}{
a \code{character} with additional attributes to look up on biomart. See \code{\link[biomaRt]{listFilters}}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
get.SYMBOLS returns a \code{data.frame} with columns corresponding to the matched gene identifiers.
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
\code{\link[biomaRt]{useMart}}, 
\code{\link[biomaRt]{listMarts}}, 
\code{\link[biomaRt]{useDataset}}, 
\code{\link[biomaRt]{listDatasets}}
}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
