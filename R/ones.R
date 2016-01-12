#' Create an array of ones or zeros
#'
#' Easy interface for creating an array of supplied dimension of ones and zeros.
#' Simply a wrapper for \code{array}. Built for convinienece and not speed.
#' 
#' @param \dots A vector or list of integers giving the dimension of the array.
#'   A dimnames argument can be supplied.
#' @return Returns an \code{array}, \code{matrix}, or \code{vector} of the 
#'   dimension specified by the arguments.
#' @seealso \code{\link{array}}
#' @examples
#' ones(6)
#' ones(3,5)
#' ones(c(2,3,3))
#' ones(3,3,3, dimnames=list(LETTERS[1:3], letters[1:3], LETTERS[4:6]))
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
#' @examples 
#' zeros(4, 3)
#' zeros(list(5, 3, 2))
#' zeros(c(3, 4), dimnames=list(LETTERS[1:3], letters[9:12]))
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
