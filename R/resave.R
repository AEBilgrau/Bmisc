#' Update or append objects to \code{.RData} file
#' 
#' Function for appending or updating objects to an existing \code{.RData} file. 
#' \code{resave} updates existing objects in an .RData file. If the 
#' resaved objects do not exist in the file, the objects are added to the 
#' file. If the file does not exist, \code{resave} functions as \code{save}.
#' @param \dots The objects to be saved, appended or overwritten.
#' @param list A character of the names to be resaved.
#' @param file A character of length 1 giving the path and \code{.RData} file 
#'   to resave the objects into.
#' @return Returns nothing. If \code{.RData}-file specified by \code{file} 
#'   does not exist \code{resave} functions exactly as \code{save} and creates 
#'   a new file. If it does exist the \code{file} is overwritten with the 
#'   updated objects. 
#' @seealso See Also: \code{\link{save}}, \code{\link{load}}
#' @note This is a modified version of a function created by user "flodel" at 
#'   \href{stack overflow.}{http://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file}
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @examples
#' file <- file.path(tempdir(), "myFile.RData") # File
#' a <- list(LETTERS) 
#'
#' resave(a, file = file)  # Save object "a"
#' 
#' b <- pi  # Create a new object "b"
#' a <- c(a, list(letters))  # Modify "a"
#' 
#' resave(a, b, file = file)  # Update a and add b
#' @export
resave <- function(..., list = character(), file) {
  if (!file.exists(file)) { # If file does not exists resave functions as save
    save(..., list = list, file = file)
  }
  previous  <- load(file)  # Returns the loaded object names
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) {
    assign(var, get(var, envir = parent.frame()))
  }
  save(list = unique(c(previous, var.names)), file = file)
}
