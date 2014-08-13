#' Wishart and inverse-Wishart distributions
#'
#' Generate data from a Wishart and inverse-Wishart distribution.
#'
#' @param sigma A square symmetric postive-definite matrix.
#' @param nu An integer giving the degrees of freedom.
#' @return \code{rwishart} returns a matrix drawn from the Wishart distribution.
#' @seealso 
#'   \code{\link{rWishart}}
#' @export
rwishart <- function(sigma, nu) {
  stopifnot(nrow(sigma) == ncol(sigma))
  p <- ncol(sigma)
  if (!(nu > p - 1)) {
    warning("For the matrix to be invertible n > p - 1 must hold")
  }
  X <- GMCM::rmvnormal(nu, mu = rep(0, nrow(V)), sigma = sigma)
  return(t(X)%*%X)
}

#' @rdname rwishart
#' @param Psi A square symmetric postive-definite matrix.
#' @export
rinvwishart <- function(Psi, nu) {
  stopifnot(nrow(Psi) == ncol(Psi))
  stopifnot(nu > ncol(Psi) - 1)
  invX <- rwishart(solve(Psi), nu)
  return(solve(invX))
}
