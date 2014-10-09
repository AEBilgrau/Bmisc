#' Test for differential correlation
#' 
#' This functions tests the hypothesis of no difference in correlations.
#' 
#' @param cor1 A numeric matrix of correlation coefficients in the first group.
#' @param cor2 As c\code{cor1} for the second group.
#' @param N1 integer of lenth 1. The number of samples in group 1.
#' @param N2 integer of lenth 1. The number of samples in group 2.
#' @return A list of matrices or vector containing:
#'   \item{\code{Z.scores}}{A numeric matrix of Z-scores for the hypothesis.}
#'   \item{\code{P.values}}{A numeric matrix of the P-values.}
#' @details 
#'   The function uses Fisher's Z transform (atanh) of correlations
#    atanh is identical to fisher's z transformation
#' @references 
#'   \url{http://core.ecu.edu/psyc/wuenschk/docs30/CompareCorrCoeff.pdf}
#' @examples
#' N1 <- 10
#' print(cor1 <- cor(t(replicate(N1, rnorm(5)))))
#' N2 <- 8
#' print(cor2 <- cor(t(replicate(N2, rnorm(5)))))
#' 
#' differential.cor.test(cor1, cor2, N1, N2)
#' @export
differential.cor.test <- function(cor1, cor2, N1, N2) {

  # Fisher Z transform the correlations
  # atanh is identical to fisher's z transformation
  Z1 <- atanh(cor1)
  Z2 <- atanh(cor2)
  diag(Z1) <- diag(Z2) <- Inf
  
  # Compute Z.scores for diff. coexpression
  Z.scores <- (Z1 - Z2)/sqrt(1/(N1 - 3) + 1/(N2 - 3))
  
  # Normalizing Z.scores
  diag(Z.scores) <- 0
  
  # Compute P-values
  P.values <- 2*pnorm(-abs(Z.scores))
  
  return(list(Z.scores = Z.scores, P.values = P.values))
}
