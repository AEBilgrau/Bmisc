#' Remove object from .RData file
#' 
#' Function to remove one or more objects from an \code{.RData} file.
#' @param rmv A \code{character} vector giving the names of the object to 
#'   remove from the specified file.
#' @param file A \code{character} of length one specifying the \code{.RData} 
#'   file to remove objects from.
#' @param \dots Arguments passed to \code{save}.
#' @author Anders E Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @examples
#' file <- paste(tempfile(), ".RData", sep = "")
#' a <- 1
#' b <- list(1:2)
#' c <- lm(rnorm(100) ~ 1)
#' save(a, b, c, file = file)
#' rm(a, b, c)
#' save.remove(c("a", "b"), file = file)
#' lst <- load(file)
#' print(lst)  # Only "c" remains
#' @export
save.remove <- function(rmv, file, ...) {
  lst <- load(file = file)
  if (!all(rmv %in% lst)) {
    warning("Some objects in rmv are not present in the .RData file.",
            "These are ignored.")
  }
  lst <- setdiff(lst, rmv)
  save(list = lst, file = file, ...)
}