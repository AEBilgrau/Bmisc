#' View left and right parts of objects
#' 
#' Extension of the functionality of \code{head} and \code{tail} with functions
#' \code{left} and \code{right} and combinations hereof.
#' Like \code{head} and \code{tail} returns the top and bottom parts of an 
#' object, \code{left} and \code{right} return the left- and right-most parts of 
#' the object. The functions are primarily oriented toward classes 
#' \code{matrix}, \code{data.frame}, and other matrix-like objects classes but 
#' are generic and may be extended to other classes if sensical.
#'
#' @param x The object.
#' @param n A single \code{integer}. If positive, the \code{n} left/right-most
#'   parts. If negative, it returns all but the \code{n} left/right-most 
#'   parts.
#' @param \dots Arguments passed to other methods.
#' @param addcolnums Logical. Should the column 'fake' numbers be added to the 
#'   output as column names?
#'   Default is \code{TRUE}.
#' @return Usually returns an object of the same class as \code{x} but small and
#'   more compact.
#' @author Modified code from \code{\link{head}} and \code{\link{tail}} and 
#'   the corresponding class-specific methods. \cr
#'   Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @seealso Main documentation of \code{\link{head}} and \code{\link{tail}}
#' @details \code{top} and \code{bot} are simply 
#' @examples
#' x <- matrix(1:100, 10, 10)
#' head(x)
#' head(x, n = -2)
#' tail(x)
#' left(x)
#' left(x, n = -2L)
#' right(x)
#' right(x, n = -2)
#' right(tail(x))
#' 
#' right(x, addcolnums = TRUE)
#' right(x, addcolnums = FALSE)
#' @rdname left.R
#' @export
left <- function(x, ...) {
  UseMethod("left")  
}

#' @rdname left.R
#' @export
left.default <- function(x, n = 6L, ...) {  # Default is head
  return(head(x, n = n, ...))
}

#' @rdname left.R
#' @export
left.matrix <- function(x, n = 6L, ...) {
  # Modified from head.matrix
  stopifnot(length(n) == 1L)
  if (n < 0L) {
    n <- max(ncol(x) + n, 0L) 
  } else {
    n <- min(n, ncol(x))
  }
  return(x[, seq_len(n), drop = FALSE])
}

#' @rdname left.R
#' @export
right <- function(x, ...) {
  UseMethod("right")  
}

#' @rdname left.R
#' @export
right.default <- function(x, n = 6L, ...) {  # Works as tail
  return(tail(x, n = n, ...))
}

#' @rdname left.R
#' @export
right.matrix <- function(x, n = 6L, addcolnums = TRUE, ...) {
  # Modified from tail.matrix
  stopifnot(length(n) == 1L) 
  ncx <- ncol(x)
  if (n < 0L) {
    n <- max(ncx + n, 0L) 
  } else {
    n <- min(n, ncx)
  }
  sel <- seq.int(to = ncx, length.out = n)
  ans <- x[, sel, drop = FALSE]
  if (addcolnums && is.null(colnames(x))) {
    colnames(ans) <- paste0("[,", sel, "]")
  }
  return(ans)
}


