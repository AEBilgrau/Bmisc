#' Estimate parameters in discriminant analysis
#' 
#' Functions to estimate the parameters in linear and quadratic
#' discriminant analysis (LDA, QDA, resp.). \code{qda2lda} provides
#' coversion from QDA to LDA.
#'
#' @rdname xda.fit
#' @aliases lda.fit qda.fit
#' @param classes A factor giving the classes of the rows of \code{var}.
#' @param vars A numeric matrix giving the observed quantities. Rows correspond
#'   to observations and columns to variables.
#' @return Returns a \code{list} with entries:
#'   \item{counts}{The number of observations in each class.}
#'   \item{means}{The estimated means for each class.}
#'   \item{sigmas}{The estimated covariance matrix for each class.}
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @export 
lda.fit <- function(classes, vars) {
  qda <- qda.fit(classes, vars)
  lda <- qda2lda(qda)
  class(lda) <- "LDA"
  return(lda)
}

#' @rdname xda.fit
#' @export
qda.fit <- function(classes, vars) {
  counts <- table(classes)
  split.vars <- split(vars, f = classes)
  means <- t(sapply(split.vars, colMeans))
  sigmas <- list()
  for (i in seq_along(counts)) {
    tmp <- as.matrix(split.vars[[i]], nrow = nrow(split.vars[[i]]))
    sigmas[[i]] <- stats::cov(tmp)
  }
  qda <- list(counts = counts, means = means, sigmas = sigmas)
  class(qda) <- "QDA"
  return(qda)
}

#' @rdname xda.fit
#' @param myqda The output of \code{lda.fit}.
#' @export
qda2lda <- function(myqda) {
  K <- length(myqda$counts)
  p <- nrow(myqda$sigmas[[1]])
  sigma <- matrix(0, p, p)
  for (i in seq_len(K)) {
    sigma <- sigma + (myqda$counts[i] - 1)*myqda$sigmas[[i]]
  }
  sigma <- sigma/(sum(myqda$counts) - K)
  myqda$sigmas <- replicate(K, sigma, simplify = FALSE)
  return(myqda)
}

#' Classify according to a discriminant analysis fit 
#' 
#' Functions to classify new data on the basis of a LDA/QDA fit.
#'
#' @rdname xda.predict
#' @aliases lda.predict qda.predict
#' @param fit A \code{list} as in the output of \code{qda.fit} or 
#'   \code{lda.fit}.
#' @param newdata A numeric matrix giving the observed quantities. Rows correspond
#'   to observations and columns to variables.
#' @return Returns a \code{list} with entries:
#'   \item{class}{A vector giving the predicted classes of the observations.}
#'   \item{posterior}{A matrix of posterior probabilities of belonging to the 
#'     classes.}
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @export 
lda.predict <- function(fit, newdata) {
  theta <- fit
  theta$means <- lapply(seq_along(theta$sigmas), function(i) theta$means[i,])
  theta$counts <- as.vector(theta$counts)/sum(theta$counts)
  theta <- c(list(m = length(theta$counts), d = nrow(theta$sigmas[[1]])), theta)
  vars <- as.matrix(newdata)
  names(theta) <- c("m", "d", "pie", "mu", "sigma")
  theta$sigma <- lapply(theta$sigma, correctForStableInversion)
  kap <- GMCM:::EStep(x = as.matrix(vars), theta = theta)
  return(list(class = apply(kap, 1, which.max), posterior = kap))
}

#' @rdname xda.predict
#' @export
qda.predict <- lda.predict


correctForStableInversion <- function(A) {
  if (rcond(A) < .Machine$double.eps) {
    eps <- .Machine$double.eps
    spec <- eigen(A)
    lambda.min <- min(spec$values)
    lambda.max <- max(spec$values)
    a <- (1e2*eps*lambda.max - lambda.min)/(1e2*eps + 1)
    spec$values <- spec$values + a
    A <- spec$vectors %*% (t(spec$vectors) * spec$values)
  }
  return(A)
}


# qda.predict <- function(myqda, newdata) {
#   theta <- myqda
#   theta$means <- lapply(seq_along(theta$sigmas), function(i) theta$means[i,])
#   theta$counts <- as.vector(theta$counts)/sum(theta$counts)
#   theta <- c(list(m = length(theta$counts), d = nrow(theta$sigmas[[1]])), theta)
#   vars <- as.matrix(newdata)
#   names(theta) <- c("m", "d", "pie", "mu", "sigma")
#   theta$sigma <- lapply(theta$sigma, correctForStableInversion)
#   kap <- GMCM:::EStep(x = as.matrix(vars), theta = theta)
#   return(list(class = apply(kap, 1, which.max), posterior = kap))
# }

# lda.predict <- function(mylda, newdata) {
#   theta <- mylda
#   K <- length(theta$counts)
#   theta$means <- lapply(seq_len(K), function(i) theta$means[i,])
#   theta$counts <- as.vector(theta$counts)/sum(theta$counts)
#   theta <- c(list(m = length(theta$counts), d = nrow(theta$sigmas[[1]])), theta)
#   vars <- as.matrix(newdata)
#   names(theta) <- c("m", "d", "pie", "mu", "sigma")
#   theta$sigma <- lapply(theta$sigma, correctForStableInversion)
#   stopifnot(is.theta(theta))
#   kap <- GMCM:::EStep(x = as.matrix(vars), theta = theta)
#   return(list(class = apply(kap, 1, which.max), posterior = kap))
# }
# 
