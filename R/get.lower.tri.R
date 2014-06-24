#' Vectorize the upper and lower triangluar matrix
#'
#' @aliases get.upper.tri
#' @param x A numeric matrix.
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
get.lower.tri <- function (x, diag = FALSE) {
  x[lower.tri(x, diag = diag)]
}

