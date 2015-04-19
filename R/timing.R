#' Easy function and evalutation timing
#' 
#' Times a function or evaulation and appends the timing results as an
#' attribute to the object.
#' 
#' @param expr A valid \proglang{R} expression or function call.
#' @return Returns the result of the evaluated \code{expr} with an attribute
#'   called \code{"timing"} containing the evaulation time.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' test <- function() mean(rnorm(1e6))
#' a <- timing(test())
#' print(a)
#' 
#' b <- timing(mean(rnorm(1e6)))
#' print(b)
#' @export
timing <- function(expr) {
  time <- system.time(expr)[3]
  names(time) <- paste(names(time), "seconds")
  attr(expr, "timing") <- time
  return(expr)
}
