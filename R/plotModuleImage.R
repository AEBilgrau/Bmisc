#' Plot the heatmap of a adjacency matrix
#'
#' @param amat An numeric adjacency matrix with values between 0 and 1.
#' @param print.xlab logical. Should the labels be shown at the x-axis?
#' @param print.ylab logical. Should the labels be shown at the y-axis?
#' @param labels character vector of labels to be printed at the x-axis and
#'   y-axis.
#' @param srt numeric. Rotation of the labels. See \code{\link{par}}.
#' @param \dots Arguments passed to \code{\link{image}}.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' # Construct a random adjacency matrix
#' amat <- replicate(10, runif(10))
#' amat[upper.tri(amat)] <- amat[lower.tri(amat)]
#' diag(amat) <- NA
#' rownames(amat) <- colnames(amat) <- paste0("Gene", 1:10)
#'
#' # Plot the graph
#' plotModuleImage(amat, srt = 30)
#' @export
plotModuleImage <- function(amat,
                            print.xlab = TRUE,
                            print.ylab = TRUE,
                            labels = rownames(amat),
                            srt = 45,
                            ...) {

  # Need to transpose (if not symmetric) and flip
  amat <- t(amat)[ncol(amat):1, ]

  # Plot
  image(x = 1:nrow(amat),
        y = 1:ncol(amat),
        z = amat,
        xlab = "",
        ylab = "",
        axes = FALSE, ...)

  if (print.xlab) {
    text(1:nrow(amat), par("usr")[1] - 0.25, labels = rev(labels),
         srt = srt, adj = 1, xpd = NA)
  }

  if (print.ylab) {
    text(par("usr")[1] - 0.25, 1:ncol(amat), labels = labels,
         srt = srt, adj = 1, xpd = NA)
  }
}
