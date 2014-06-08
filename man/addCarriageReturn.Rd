\name{addCarriageReturn}
\alias{addCarriageReturn}
\title{
Add Carriage Return
}
\description{
Unix system only use different characters for the newline. Windows uses carriage return. This function adds the carriage return.
}
\usage{
addCarriageReturn(file.in, file.out = file.in)
}
\arguments{
\item{file.in}{
The path and name of the input file. A character of length 1.
}
\item{file.out}{
The path and name of the output file. A character of length 1. Default is to overwrite.
}
}
\value{
Creates (or overwrites if file.out is not given) a file with added carriage returns.
}
\author{
Anders Ellern Bilgrau <abilgrau@math.aau.dk>
}
\examples{
tmp.dir <- tempdir()
file.in <- file.path(tmp.dir, "NewFile.txt")
file.out <- file.path(tmp.dir, "NewFileForWindows.txt")

# Write the file
cat("This is some text on.\n Hello, newline.", 
    file = file.in)

# Add carriage return if on a unix system
addCarriageReturn(file.in, file.out)

# If on unix, run to see files:
#system(paste("open", file.in))
#system(paste("open", file.out))
}
\keyword{ Carriage Return }
\keyword{ Windows txt }% __ONLY ONE__ keyword per line