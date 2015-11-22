#' The minimum absolute first order partial correlation
#' 
#' This functions computes all first order partial correlations and retuns
#' the partial correlation with the least absolute size.
#' 
#' @param S A numeric covariancematrix 
#' @return A numeric matrix.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @seealso \code{\link{pcor}}
#' @examples
#' X <- replicate(3, rnorm(5))
#' dimnames(X) <- list(paste0("obs", 1:nrow(X)), paste0("var", 1:ncol(X)))
#' S <- cor(X)
#' minimum.abs.pcor(S)
#' @export
minimum.abs.pcor <- function(S) {
  current.min <- pcor(S, k = 1)
  current.min[is.nan(current.min)] <- 1
  for (k in 2:nrow(S)) {
    new.min <- pcor(S, k = k)
    new.min[is.nan(new.min)] <- 1
    is.new.min <- abs(new.min) < abs(current.min)
    current.min[is.new.min] <- new.min[is.new.min]
  }
  return(current.min)
}
