#' Vectorize the upper and lower triangluar matrix
#'
#' Get and set the upper and lower triangular matrix in vector form.
#' 
#' @aliases get.upper.tri
#' @param x A \code{matrix}.
#' @param diag logical. Should the diagonal be included?
#' @return \code{get.lower.tri} returns a numeric vector of the entries in the
#'   below the diagonal of \code{x}. The length is dependent on argument 
#'   \code{diag}.
#' @details The returned length is dependent on \code{diag}. If 
#'   \code{diag == TRUE}, then the length is \code{ncol(x)*(ncol(x)-1)/2}. If 
#'   \code{FALSE}, then the length is \code{ncol(x)*(ncol(x)+1)/2}.
#' @seealso \code{\link{lower.tri}}, \code{\link{upper.tri}}
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @export
#' @examples
#' print(x <- matrix(1:25, ncol = 5))
#' get.lower.tri(x)
#' get.lower.tri(x, diag = TRUE)
#' 
#' print(y <- matrix(1:9, ncol = 3))
#' get.upper.tri(y)
#' get.upper.tri(y, diag = TRUE)
#' 
#' get.upper.tri(y) <- get.lower.tri(y)  # Fill the upper tri with the lower
#' print(y)
#' get.upper.tri(y)
#' @export
get.lower.tri <- function (x, diag = FALSE) {
  return(x[lower.tri(x, diag = diag)])
}

#' @rdname get.lower.tri
#' @param value A vector of the same data type a \code{x} of the same length
#'   as \code{get.lower.tri}.
#' @export
`get.lower.tri<-` <- function(x, value, diag = FALSE) {
  x[lower.tri(x, diag = diag)] <- value
  return(x)
}

#' @rdname get.lower.tri
#' @return \code{get.upper.tri} returns a numeric vector of the entries in the
#'   below the diagonal of \code{x}. The length is dependent on argument 
#'   \code{diag}.
#' @export
get.upper.tri <- function (x, diag = FALSE) {
  return(x[upper.tri(x, diag = diag)])
}

#' @rdname get.lower.tri
#' @export
`get.upper.tri<-` <- function(x, value, diag = FALSE) {
  x[upper.tri(x, diag = diag)] <- value
  return(x)
}

