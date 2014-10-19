#' Perform rowwise Welch t-tests.
#' 
#' This function performs rowwise Welch two-sample t-test in two 
#' matrices with the same height in a fast manner.
#' 
#' @param x A numeric matrix of the values of the first group. Usually the 
#'   expression matrix of the first group.
#' @param y A numeric matrix of the values in the second group with the same 
#'   number of rows as \code{x}.
#' @return A \code{data.frame} with the same number of rows as x containing the 
#'   means, variances, pooled stadard 
#'   deviations, estimated difference, the standard error of the difference,
#'   the t statistic, the degrees of freedom, and the p-values.
#'   
#' @details A Welch t-test is performed for each row in \code{x} and \code{y}. 
#'   The \eqn{i}'th row of \code{x} is tested against the \eqn{i}'th row of 
#'   \code{y} and the result is seen in the \eqn{i}'th row of the output.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' # Create some toy data
#' x <- replicate(10, rnorm(20, 0, 1))
#' y <- replicate(11, c(rnorm(10, 1, 1), rnorm(10, 0, 1)))
#' rownames(x) <- rownames(y) <- paste0("Gene", 1:20)
#' 
#' welch.t.tests(x, y)
#'
#' # To perform a single t test
#' t.test(x[1,], y[1, ])
#' welch.t.tests(x[1, , drop = FALSE], y[1, , drop = FALSE])
#' @export
welch.t.tests <- function(x, y) {
  if (!(is.matrix(x) & is.numeric(x) & is.matrix(y) & is.numeric(y))) {
    stop("Both x and y must be numeric matrices.")
  }
  if (nrow(x) != nrow(y)) {
    stop("The number of rows in x and y must equal.")
  }
  nx <- ncol(x)
  ny <- ncol(y)
  meanx <- rowMeans(x)
  meany <- rowMeans(y)
  diff  <- meanx - meany
  varx  <- rowSums((x - meanx)^2)/(nx - 1)
  vary  <- rowSums((y - meany)^2)/(ny - 1)
  sd.pool <- ((nx - 1)*(varx)+(ny - 1)*vary)/(nx + ny - 2)
  var   <- varx/nx + vary/ny
  se    <- sqrt(var)
  t     <- diff/se
  df    <- var^2/( varx^2/(nx^2*(nx - 1)) + vary^2/(ny^2*(ny - 1)) )
  pval  <- 2*pt(-abs(t), df = df) 
  
  return(data.frame(meanx, meany, varx, vary, sd.pool, diff, se, t, df, pval))
}

