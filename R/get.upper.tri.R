#' @rdname get.lower.tri
#' @return \code{get.upper.tri} returns a numeric vector of the entries in the
#'   below the diagonal of \code{x}. The length is dependent on argument 
#'   \code{diag}.
#' @export
#' @examples
#' print(x <- matrix(1:9, ncol = 3))
#' get.upper.tri(x)
#' get.upper.tri(x, diag = TRUE)
get.upper.tri <- function (x, diag = FALSE) {
  x[upper.tri(x, diag = diag)]
}

