#' Plot matrix as heat map
#'
#' Plots a \code{matrix} as a heat map/image with the correct positions. Simply
#' a wrapper for \code{\link{image}}. It flips and transposes the matrix
#' in image it as it is printed. Good for visualizing large matrices.
#' 
#' @param x A \code{matrix} to plot.
#' @param \dots Arguments passed to \code{\link{image}}.
#' @return Invisibly returns the output of \code{image}.
#' @author Anders Ellern Bilgrau
#' @seealso \code{\link{image}}
#' @examples
#' y <- matrix(rnorm(200), 20, 10)
#' plotMatrix(y)
#' 
#' z <- matrix(seq_len(200), 20, 10)
#' plotMatrix(z)
#' @export
plotMatrix <- function(x, ...) {
  xx <- seq_len(ncol(x))
  yy <- seq_len(nrow(x))
  zz <- t(x)[, rev(yy)]
  return(invisible(image(xx, yy, z = zz, ...)))
}
