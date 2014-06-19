#' @rdname rowSds
#' @return \code{colSds} returns a numeric vector of length \code{m}.
#' @examples
#' y <- matrix(rnorm(50), 10, 5)
#' colSds(y)
#' @export
rowSds <- function(x) {
  n <- ncol(x)
  means <- rowMeans(x)
  return(sqrt(rowMeans((x - means)^2)*(n/(n - 1))))
}

