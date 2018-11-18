#' Plot a matrix of colors
#'
#' Plot a \code{character} matrix giving the colours to plot.
#' 
#' @param x A \code{character} \code{matrix} of colour names.
#' @param add \code{logical}. Should the matrix be added to the current plot 
#'   device.
#' @param \dots Arguments passed to \code{\link{rect}}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @return
#'   Invisibly returns the x and y coordinates and the corresponding colours.
#' @details The colours are plotted as squares as given by \code{x} on the grid
#'   of integers. The entry given in \code{x[i,j]} is plotted as a rectangle
#'   at coordinate (\code{i}, \code{nrow(x)-j}).
#' @examples
#' plotColourMat()  # Plots a random 6 by 10 colour matrix.
#' 
#' x <- matrix(sample(colors(), 20), ncol = 5)
#' plotColourMat(x)
#' 
#' x <- matrix(rainbow(100), 1, 100)
#' plotColourMat(x)
#' dim(x) <- c(10, 10)
#' plotColourMat(x)
#' 
#' y <- matrix(sample(colors(), 25), ncol = 5)  
#' plotColourMat(y, add = TRUE)
#' axis(1)
#' axis(2)
#' @importFrom graphics rect plot 
#' @importFrom grDevices colors
#' @export
plotColourMat <- function(x, add = FALSE, ...) {
  if (missing(x)) {
    x <- matrix(sample(colors())[1:(6*10)], 6, 10)
  }
  nr <- nrow(x)
  nc <- ncol(x)
  if (!add) {
    plot(1, type = "n", xlab = "", ylab = "", axes = FALSE,
         xlim = c(.5, nc + .5), ylim = c(.5, nr + .5))
  }
  xx <- rep(seq_len(nc), each = nr)
  yy <- rep(seq(nr, 1), nc)
  col <- c(x)
  rect(xx - 0.5, yy - 0.5, xx + 0.5, yy + 0.5, col = col, border = NA, ...)
  return(invisible(list(x = xx, y = yy, col = col)))
}