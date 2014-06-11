\name{resave}
\alias{resave}
\title{
Update objects in .RData files
}
\description{
\code{resave} updates existing objects in an .RData file. If the resaved objects do not exist in the file, the objects are added to the file. If the file does not exist, \code{resave} functions as \code{save}.
}
\usage{
resave(..., list = character(), file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
  The R objects to be resaved.
}
  \item{list}{
  A character of the names of the optional list of names to be resaved.
}
  \item{file}{
  The \code{.RData} file to save the object into.
}
}
%\details{
%The \code{resave} function resaves
%}
\value{
The \code{.RData}-file specified by \code{file} is written or overwritten to the harddrive. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anders Ellern Bilgrau
}
\note{
This is a modified version of a function created by user "flodel" at \href{stack overflow.}{http://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file}

}


\seealso{
See Also: \code{\link{save}}
}
\examples{

file <- file.path(tempdir(), "/myFile.RData")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
