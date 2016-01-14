#' Flip vector names and values
#' 
#' Simply flips or switches values and names of named vectors. This is e.g. 
#' useful when named character vectors are used a mapping.
#' 
#' @param x A named vector.
#' @return A named character vector.
#' @author Anders Ellern Bilgrau
#' @examples 
#' x <- structure(1:5, names = LETTERS[1:5])
#' x
#' flip(x)
#' flip(flip(x))
#' 
#' 
#' y <- structure(c("a", "b", "c"), names = c("abel", "bob", "charlie"))
#' y["bob"]
#' flip(y)["c"]
#' @export
flip <- function(x) {
  stopifnot(is.vector(x))
  ans <- names(x)
  names(ans) <- x
  return(ans)
}
