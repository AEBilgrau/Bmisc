#' Create an array of ones, zeros
#'
#' Simple functions for creating an array of ones or zeros. Simply a wrapper
#' for \code{array}.
#' Built for convinienece and not speed.
#' 
#' @param \dots A list of the number of ones/zeros along each dimension.
#'   Can also be an integer vector.
#' @return Returns an \code{array} of the dimension specified.
#' @seealso \code{\link{array}}
#' @examples
#' ones(10)
#' ones(10,5)
#' ones(c(2,3,3))
#' ones(3,3,3, dimnames=list(LETTERS[1:3], letters[1:3], LETTERS[4:6]))
#' 
#' zeros(10, 12)
#' zeros(c(5, 3, 2))
#' zeros(3, 4, dimnames=list(LETTERS[1:3], letters[9:12]))
#' @export
ones <- function(...) {
  args <- list(...)
  is.dimnames <- names(args) %in% "dimnames"
  if (any(is.dimnames)) {
    dimnames <- args$dimnames
    args <- args[!is.dimnames]
  }
  return(array(1, dim = unlist(args), dimnames = dimnames))
}

#' @rdname ones
#' @export
zeros <- function(...) {
  args <- list(...)
  is.dimnames <- names(args) %in% "dimnames"
  if (any(is.dimnames)) {
    dimnames <- args$dimnames
    args <- args[!is.dimnames]
  }
  return(array(0, dim = unlist(args), dimnames = dimnames))
}
