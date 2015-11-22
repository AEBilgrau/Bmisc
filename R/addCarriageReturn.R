#' Add Carriage Return
#' 
#' Unix based systems use only the line feed (\code{LF}, \code{"\\n"}) character 
#' for line breaks whereas Windows both line feed and carriage return 
#' (\code{CR}, \code{"\\r"}). This function adds the carriage return in order
#' for files to be read properly on windows.
#' 
#' @param file.in The path and name of the input file. A character of length 1.
#' @param file.out The path and name of the output file. A character of 
#'   length 1. Default is to overwrite.
#' @return Nothing is returned. Creates (or overwrites if file.out is not 
#'   given) a file with added carriage returns.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @note Useful on Unix systems to make Windows readable \code{.txt} and other
#'   plain-text files.
#' @examples
#' tmp.dir <- tempdir()
#' file.in <- file.path(tmp.dir, "NewFile.txt")
#' file.out <- file.path(tmp.dir, "NewFileForWindows.txt")
#'
#' # Write the file
#' cat("This is some text on.\n Hello, newline.", file = file.in)
#'
#' # Add carriage return if on a unix system
#' addCarriageReturn(file.in, file.out)
#'
#' # If on unix, run to see files:
#' #system(paste("open", file.in))
#' #system(paste("open", file.out))
#' @export
addCarriageReturn <- function(file.in, file.out = file.in) {
  file.in <- path.expand(file.in)
  file.out <- path.expand(file.out)
  if (.Platform$OS.type == "unix") {
    system(paste("sed -e 's/$/\r/' ", file.in, ">", file.out))  
  }
}
