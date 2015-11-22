#' Plot a colour key
#'
#' Easy plotting of a colour key.
#' 
#' @param breaks A numeric vector of length \eqn{n} 
#'   giving the breakpoints of the key.
#' @param col A character vector of the colours used of length \eqn{n - 1}.
#' @param add Should the key be added to the current plot device?
#' @param \dots Arguments passed to \code{\link{rect}}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @examples
#' brks <- seq(0, 1, length.out = 8)
#' cls <- rainbow(7)
#' plotColourKey(brks, cls)
#' axis(1)
#' axis(2)
#' @export
plotColourKey <- function(breaks, col, add = FALSE, ...) {
  stopifnot(length(col) + 1 == length(breaks))
  nb <- length(breaks)
  if (!add) {
    plot(breaks, xlim = range(breaks), ylim = c(0,1), type = "n", axes = FALSE,
         xlab ="", ylab = "")
  }
  rect(breaks[-nb], 0, breaks[-1], 1, col = col, border = NA, ...)
}
