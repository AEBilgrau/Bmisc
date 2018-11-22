#' Flip vector names and values
#' 
#' Simply flips or switches values and names of named vectors. This is e.g. 
#' useful when named character vectors are used a mapping.
#' 
#' @param x A named vector.
#' @param convert logical. If \code{TRUE}, an attempt will be made to coerce 
#'   the flipped values into a \code{numeric}.
#' @return A named character vector.
#' @author Anders Ellern Bilgrau
#' @examples 
#' x <- structure(1:5, names = LETTERS[1:5])
#' x
#' flip(x)
#' flip(flip(x))
#' 
#' y <- structure(c("a", "b", "c", "d", "e", "f", "g"), 
#'    names = c("albert", "bob", "charlie", "dennis", "eddie", "frank", "hugo"))
#' y["hugo"]
#' flip(y)["a"]
#' 
#' z <- sample(c(TRUE, FALSE), size = 10, replace = TRUE)
#' names(z) <- LETTERS[seq_along(z)]
#' z
#' flip(z)
#' flip(flip(z))
#' @importFrom utils hasName
#' @export
flip <- function(x, convert = TRUE) {
  stopifnot(is.vector(x) & hasName(attributes(x), "names"))
  ans <- names(x)
  if (convert) {
    suppressWarnings(tmp_numeric <- as.numeric(ans))
    if (!identical(tmp_numeric, rep(NA_real_,length(ans)))) {
      ans <- tmp_numeric
    }
  }
  names(ans) <- x
  return(ans)
}
