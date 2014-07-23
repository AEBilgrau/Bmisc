#' Find differential networks between two classes
#'
#' This function attemps to find differential networks between two classes.
#' First, it computes the partial correlation (either unconditioned, the
#' smallest first order conditioned, or full conditioned) for each group.
#' Second it tests each partial correlation for differential correlation using
#' Fishers Z transform and the normal distribution. If wanted, the topological
#' overlap matrix is computed from the resulting matrix of P-values.
#'
#' @param x is the expression matrix where columns correspond to
#'   samples/subjects and rows to genes.
#' @param class is the factor enconding ABC and GCB samples
#' @param gene.universe character vector giving the names of the features to be
#'   used. I.e. a subset of \code{colnames(x)}.
#' @param name A character giving the name of the dataset.
#' @param pcor.type character of length one. The type of correlation to be
#'   used. Equal to one of "marginal", "partial", or "fullpartial".
#' @param use.TOM logical. Should the topological overlap matrix be computed?
#'   of the P-values of the Fisher transformed tests?
#' @param rho Non-negative numeric regularization parameter for lasso. rho = 0
#'   means no regularization. Only used if pcor.type == "fullpartial"
#' @return A list of containing the output (to be described better)
#' @details If \code{pcor.type} is \code{"partial"} then all 1 order partial
#'   correlations are computed (i.e. the partial correlation given a third
#'   feature) and the minimal partial correlation is used.
#' @note The toplogical overlap matrix is computed on the P-values.
#' @export
computeWeigtedDiffNetwork <- function(x,
                                      class,
                                      gene.universe,
                                      name = "datasetname",
                                      pcor.type = c("marginal",
                                                    "partial",
                                                    "fullpartial"),
                                      use.TOM = TRUE,
                                      rho = 0,
                                      recompute) {
  pcor.type <- match.arg(pcor.type)
  if (missing(recompute)) {
    recompute <- FALSE
  }

  cat("Estimating network of", name, "using", pcor.type, "correlations.\n")

  # x.abc and x.gcb are the case and control groups
  x.abc <- t(x[gene.universe, class == "ABC"])
  x.gcb <- t(x[gene.universe, class == "GCB"])

  N.abc <- nrow(x.abc)
  N.gcb <- nrow(x.gcb)

  # Compute marginal correlations
  covariance.abc <- cov(x.abc)
  covariance.gcb <- cov(x.gcb)

  # Compute partial correlations (0'th order, min 1'st order, or Ni'th order)
  mycor <- function(covariance.mat, # The correlation matrix
                   pcor.type,   # Type of correlation to compute
                   rho = 0) {   # If glasso is used, the level of regularization
    if (pcor.type == "fullpartial") {
      if (!require("glasso")) {
        stop("The glasso package is needed")
      }
    } else {
      correlation.mat <- cov2cor(covariance.mat)
    }
    switch(pcor.type,
           "marginal"    = correlation.mat,
           "partial"     = min.abs.pcor(correlation.mat),
           "fullpartial" = cov2cor(glasso(covariance.mat, rho = rho)$w))
  }

  pcorrelation.abc <- mycor(covariance.abc, pcor.type = pcor.type, rho = rho)
  pcorrelation.gcb <- mycor(covariance.gcb, pcor.type = pcor.type, rho = rho)

  dimnames(pcorrelation.abc) <- dimnames(covariance.abc)
  dimnames(pcorrelation.gcb) <- dimnames(covariance.gcb)

  diag(pcorrelation.abc) <- 1
  diag(pcorrelation.gcb) <- 1

  #
  # Test for diff. coexpression
  # See. http://core.ecu.edu/psyc/wuenschk/docs30/CompareCorrCoeff.pdf
  #

  # Function to transform correlation coefficients
  diff.cor.test <- function(cor1, cor2, N1, N2) {
    #x1 and x2 are correlation (matrices)
    #N1 and N2 are the sample sizes

    # Fisher Z transform the correlations
    # atanh is identical to fisher's z transformation
    Z1 <- atanh(cor1)
    Z2 <- atanh(cor2)
    diag(Z1) <- diag(Z2) <- Inf

    # Compute Z.scores for diff. coexpression
    # (N - 4 and not -3 because of partial correlaiton)
    Z.scores <- (Z1 - Z2)/sqrt(1/(N1 - 4) + 1/(N2 - 4))

    # Normalizing Z.scores
    #Z.scores <-
    #  (Z.scores - mean(Z.scores, na.rm = TRUE))/sd(Z.scores, na.rm = TRUE)
    diag(Z.scores) <- 0

    # Compute P-values
    P.values <- 2*pnorm(-abs(Z.scores))

    # Similarity matrix
    similarity <- 1 - P.values

    return(list(Z.scores = Z.scores, similarity = similarity))
  }

  cat("Testing for differential coexpression\n")
  diff.cor <- diff.cor.test(pcorrelation.abc,
                            pcorrelation.gcb,
                            N.abc, N.gcb)

  # Adjacency matrix
  cat("Computing adjacency matrix\n"); flush.console()
  adjacency <- diff.cor$similarity

  # Topological overlap
  tom.dissimilarity <- NULL
  if (use.TOM) {
    cat("Computing TOM\n")
    tom.similarity    <- TOMsimilarity(adjacency, TOMType = "unsigned")
    tom.dissimilarity <- 1 - tom.similarity
    dimnames(tom.dissimilarity) <- dimnames(adjacency)
  }

  # Return the results
  return(list(Z.scores = diff.cor$Z.scores,
              adjacency = adjacency,
              tom.dissimilarity = tom.dissimilarity))
}
