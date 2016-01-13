#' Flip vector names and values
#' 
#' Flips or switches names and values of vectors.
#' 
#' @param x A named vector.
#' @return A character vector.
#' @author Anders Ellern Bilgrau
#' @examples 
#' x <- structure(1:5, names = LETTERS[1:5])
#' x
#' flip(x)
#' flip(flip(x))
#' @export
flip <- function(x) {
  stopifnot(is.vector(x))
  ans <- names(x)
  names(ans) <- x
  return(ans)
}
