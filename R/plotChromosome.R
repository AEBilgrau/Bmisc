#' Plot a chromosome
#' 
#' \code{plotChromosome} draws a chromosome and cytobands with an interface 
#' akin to \code{\link{rect}} with the given coordinates.
#'
#' @param chromosome A character giving the chromosome to plot. Should be one 
#'   of \code{"chr1"}, ..., \code{"chr22"}, \code{"chrX"}, or \code{"chrY"}.
#' @param xleft A scalar giving the left x position.
#' @param ybot A scalar giving the bottom y postion.
#' @param xright A scalar giving the right x postion.
#' @param ytop A scalar giving the top y postion.
#' @param taper A numeric givning the amount of tapering near the ends of the 
#'   chromosome and the centromere.
#' @param build Which UCSC genome build to use. 
#'   Supported builds are \code{"hg18"} and \code{"hg19"}.
#' @param \dots Arguments passed to \code{\link{polygon}}.
#' @note
#'   Some of the code is borrowed from \code{\link[SNPchip]{plotIdiogram}} in 
#'   the \code{SNPchip} package by Robert Scharpf and Jason Ting.
#' @author
#'   Anders Ellern Bilgrau \cr
#'   Modified code by Robert Scharpf and Jason Ting 
#' @seealso 
#'   \code{\link{rect}}, \code{\link[SNPchip]{plotIdiogram}},
#'   \code{\link[oligoClasses]{getSequenceLengths}}
#' @examples
#' if (require("SNPchip")) {
#'   dat <- cumsum(rnorm(100))/sqrt(seq_len(100))
#'   plot(dat, ylab = "Something", type = "h", ylim = c(-3, 3), 
#'        col = ifelse(dat > 0, "red", "blue"), lwd = 2)
#'   plotChromosome("chr10", xleft = 0, ybot = -3, xright = 100, ytop = -2.8)
#' }
#' @importFrom oligoClasses getSequenceLengths
#' @importFrom SNPchip getCytoband
#' @importFrom graphics polygon segments
#' @export
plotChromosome <- function(chromosome = "chr1",
                           xleft = 0,
                           ybot = 0,
                           xright = 1,
                           ytop = 1,
                           taper = 0.2,
                           build = "hg19", ...) {

  stopifnot(requireNamespace("oligoClasses"))
  stopifnot(requireNamespace("SNPchip"))

  cytoband <- getCytoband(build)
  cytoband <- cytoband[cytoband[, "chrom"] == chromosome, ]
  rownames(cytoband) <- cytoband[,"name"]
  sl <- getSequenceLengths(build)[chromosome]

  cytoband_p <- cytoband[grep("^p", rownames(cytoband), value = TRUE), ]
  cytoband_q <- cytoband[grep("^q", rownames(cytoband), value = TRUE), ]
  p.bands <- nrow(cytoband_p)

  cut.left <- c()
  cut.right <- c()
  N <- nrow(cytoband)
  for (i in seq_len(N)) {
    if (i == 1) {
      cut.left[i]  <- TRUE
      cut.right[i] <- FALSE
    } else if (i == p.bands) {
      cut.left[i]  <- FALSE
      cut.right[i] <- TRUE
    } else if (i == (p.bands + 1)) {
      cut.left[i]  <- TRUE
      cut.right[i] <- FALSE
    } else if (i == nrow(cytoband)) {
      cut.left[i]  <- FALSE
      cut.right[i] <- TRUE
    } else {
      cut.left[i]  <- FALSE
      cut.right[i] <- FALSE
    }
  }

  for (i in seq_len(N)) {
    if (as.character(cytoband[i, "gieStain"]) == "stalk") {
      cut.right[i - 1] <- TRUE
      cut.left[i]      <- NA
      cut.right[i]     <- NA
      cut.left[i + 1]  <- TRUE
    }
  }

  getStain <- function(stain) {
    switch(stain, gneg = "grey100", gpos25 = "grey90",
           gpos50 = "grey70", gpos75 = "grey40", gpos100 = "grey0",
           gvar = "grey100", stalk = "brown3", acen = "brown4", "white")
  }

  h <- ytop - ybot
  p <- taper

  for (i in seq_len(N)) {

    start <- (cytoband[i, "start"]/sl)*(xright - xleft) + xleft
    last <-  (cytoband[i, "end"]/sl)*(xright - xleft) + xleft

    delta = (last - start)/4
    color <- getStain(as.character(cytoband[i, "gieStain"]))
    if (is.na(cut.left[i]) & is.na(cut.right[i])) {
      delta <- (last - start)/3
      segments(start + delta, ybot,
               start + delta, ytop, ...)
      segments(last - delta, ybot,
               last - delta, ytop, ...)
    } else if (cut.left[i] & cut.right[i]) {
      yy <- c(ybot + p * h, ybot, ybot, ybot + p * h, ytop -
                p * h, ytop, ytop, ytop - p * h)
      polygon(c(start, start + delta, last - delta, last,
                last, last - delta, start + delta, start), yy,
              col = color, ...)
    } else if (cut.left[i]) {
      yy <- c(ybot + p * h, ybot, ybot, ytop, ytop, ytop - p * h)
      polygon(c(start, start + delta, last, last, start + delta, start), yy,
              col = color, ...)
    } else if (cut.right[i]) {
      yy <- c(ybot, ybot, ybot + p * h, ytop - p * h, ytop,
              ytop)
      polygon(c(start, last - delta, last, last, last -
                  delta, start), yy, col = color, ...)
    } else {
      polygon(c(start, last, last, start),
              c(ybot, ybot, ytop, ytop), col = color, ...)
    }
  }
}
