#' Multivariate special functions
#'
#' Special matematical functions related to the multivariate gamma function
#' which appears in the Wishart and Inverse Wishart distributions. The interface
#' and usage should mirror the \link{Special} mathematical functions from the
#' base package.
#'
#' @rdname MultivariateSpecial
#' @aliases MultivariateSpecial
#' @param x A numeric vector of values. The values of \code{x} should be 
#'   strictly greater than \code{(p - 1)/2}.
#' @param p A (single) integer giving the dimension. Default is \code{1}.
#' @param deriv A (single) integer giving the order of the derivative. 
#'   Default is \code{0}.
#' @return Returns a numeric vector with the same length as \code{x}.
#'   Similar to \code{\link{gamma}} and related functions \code{NaN}s are 
#'   returned with warning when the function is evaluated outside its domain
#'   
#'   \code{gammap} is the multivariate gamma function. 
#' @details
#'   Warnings are thown when the functions are evaluated outside its domain.
#'   Run \code{Bmisc:::plotMultivariateSpecial()} for more info.
#' @author
#'   Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @seealso The \link{Special} mathematical functions.
#' @examples
#' gammap(3.2, p = 1)
#' gammap(c(0.2, 0.5, 0.9, 1.2, 1.5), p = 3)   # x < (p-1)/2  !
#' lgammap(3.1, p = 2)
#' digammap(2, p = 3)
#' Dgammap(3.2, p = 1)
#' 
#' # A plot of all special functions
#' Bmisc:::plotMultivariateSpecial()
#' @export
gammap <- function(x, p = 1) {
  stopifnot(length(p) == 1)
  t <- pi^(p*(p - 1)/4)
  if (any((p - 1)/2 > x)) {
    warning("NaNs produced")
  }
  suppressWarnings({
    terms <- sapply(seq_len(p) - 1, function(j) gamma(x - j/2))
    dim(terms) <- c(length(x), p)
    return(t * exp(rowSums(log(terms))))  #== apply(terms, 1, prod)
  })
}

#' @rdname MultivariateSpecial
#' @return \code{digammap} is the multivariate digamma function. 
#' @export
digammap <- function(x, p = 1) {
  stopifnot(length(p) == 1)
  terms <- sapply(seq_len(p) - 1, function(j) digamma(x - j/2))
  dim(terms) <- c(length(x), p)
  return(rowSums(terms))
}

#' @rdname MultivariateSpecial
#' @return \code{Dgammap} is the derivative of the multivariate gamma function.
#' @export
Dgammap <- function(x, p = 1) {
  return(gammap(x, p)*digammap(x,p))
}

#' @rdname MultivariateSpecial
#' @return \code{psigammap} is the \eqn{n}'th derivative log multivariate 
#'   gamma function (i.e. multivariate polygamma).
#' @export
psigammap <- function(x, p = 1, deriv = 0) {
  stopifnot(length(p) == 1)
  terms <- sapply(seq_len(p) - 1, function(j) psigamma(x - j/2, deriv = deriv))
  dim(terms) <- c(length(x), p)
  return(rowSums(terms))
}

#' @rdname MultivariateSpecial
#' @return \code{trigammap} is the \eqn{2}'nd derivative log multivariate 
#'   gamma function (i.e. the multivariate trigamma).
#' @export
trigammap <- function(x, p = 1, deriv = 0) {
  stopifnot(length(p) == 1)
  terms <- sapply(seq_len(p) - 1, function(j) trigamma(x - j/2))
  dim(terms) <- c(length(x), p)
  return(rowSums(terms))
}

plotMultivariateSpecial <- function() {
  # A nice plot of the functions
  par(mfrow = c(2,2), mar =  c(1, 2, 1.3, -1) + 1.1)
  up <- 4
  x <- seq(0.01, up, by = 0.01)
  max.p <- 4
  
  # Gamma_p
  plot(x, type = "n", col = 2, lwd = 2, axes = FALSE, xlab = "", ylab = "",
       ylim = c(0, 1)*100, xlim = c(0, 1)*up,
       main = expression(plain(gammap(x, p)) :~~ Gamma[p](x)))
  for (j in seq_len(max.p)) {
    curve(gammap(x, p = j), add = TRUE, lwd = 2,
          col = j, xlim = c((j - 1)/2, up)  + .Machine$double.eps, n = 201)
  }
  abline(v = (seq_len(max.p)-1)/2, col = seq_len(max.p), lty = 2)
  axis(1); axis(2)
  
  # log Gamma_p
  plot(x, type = "n", col = 2, lwd = 2, ylim = c(0, 10), axes = FALSE,
       ylab = "", xlab = "", xlim = c(0, up), 
       main = expression(plain(lgammap(x, p)) :~~ log * Gamma[p](x)))
  for (j in seq_len(max.p)) {
    curve(lgammap(x, p = j), col = j, add = TRUE, lwd = 2,
          xlim = c((j - 1)/2, up) + .Machine$double.eps)
  }
  abline(v = (seq_len(max.p)-1)/2, col = seq_len(max.p), lty = 2)
  axis(1); axis(2)
  
  
  # derivative of gamma_p
  plot(x, type = "n", col = 2, lwd = 2, xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 4), ylim = c(-1, 1)*25,
       main = expression(plain(Dgammap(x, p)) :~~ frac(d, d*x)*Gamma[p](x)))
  for (j in seq_len(max.p)) {
    curve(Dgammap(x, p = j), col = j, add = TRUE, lwd = 2,
          xlim = c((j - 1)/2, up) + .Machine$double.eps)
  }
  abline(v = (seq_len(max.p)-1)/2, col = seq_len(max.p), lty = 2)
  axis(1); axis(2)
  
  
  # digamma_p
  main <- 
    expression(plain(digammap(x,p)):~~frac(d,d*x)*log*Gamma[p](x)==psi[p](x))
  plot(x, type = "n", col = 2, lwd = 2, xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 4), ylim = c(-10, 5), main = main)
  for (j in seq_len(max.p)) {
    curve(digammap(x, p = j), col = j, add = TRUE, lwd = 2,
          xlim = c((j - 1)/2, up) + .Machine$double.eps)
  }
  abline(v = (seq_len(max.p)-1)/2, col = seq_len(max.p), lty = 2)
  axis(1); axis(2)
  
  legend("bottomright", col = seq_len(max.p), lty = 1, lwd = 2, bty = "n",
         legend = paste("p =", seq_len(max.p)), inset = 0.05)
}
