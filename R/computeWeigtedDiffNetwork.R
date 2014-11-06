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
#' @return A list of length 3:
#'   \item{Z.scores}{A matrix of Z-scores for differential (partial)
#'     correlation.}
#'   \item{adjacency}{A matrix of 1 - P-values for the hypothesis of no
#'     differential (partial) correlation}
#'   \item{tom.dissimilarity}{A matrix of topological overlap disimilarities.}
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
                                      rho = 0) {
  pcor.type <- match.arg(pcor.type)

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
           "partial"     = minimum.abs.pcor(correlation.mat),
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
  #

  cat("Testing for differential coexpression\n")
  diff.cor <- differential.cor.test(pcorrelation.abc,
                                    pcorrelation.gcb,
                                    N.abc,
                                    N.gcb)

  # Adjacency matrix
  cat("Computing adjacency matrix\n"); flush.console()
  adjacency <- 1 - diff.cor$P.values

  # Topological overlap
  tom.dissimilarity <- NULL
  if (use.TOM && require("WGCNA")) {
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
