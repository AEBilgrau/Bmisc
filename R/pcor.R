#' Compute first order partial correlations
#'
#' This function computes the first order partial correlation given the
#' \code{k}'th variable.
#' @param X an numeric correlation matrix
#' @param k an integer. The variable to condition on.
#' @return A matrix of the same size as \code{X} with \code{NaN}s in the
#'   \code{k} row and column.
#' @seealso \link{\code{cor}}.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' mat <- replicate(4, rnorm(10))
#' X <- cor(mat)
#' pcor(X, 2)
pcor <- function(X, k) {
  tmp <- sqrt(1 - X[k, ]^2)
  return((X - tcrossprod(X[k, ]))/tcrossprod(tmp))
}
