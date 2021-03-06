% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fillLines.R
\name{fillLines}
\alias{fillLines}
\title{Fill the area between lines}
\usage{
fillLines(x, y1, y2 = rep(0, length(x)), ...)
}
\arguments{
\item{x}{A numeric vector of x-coordinates.}

\item{y1}{A numeric vector of y-coordinates.}

\item{y2}{A numeric vector of y-coordiantes. Defaults to zero.}

\item{\dots}{Arguments passed to \code{polygon}.}
}
\description{
This function fills or shades the area between two curves or under a curve.
}
\details{
The function fills or shades the area between two lines as drawn by 
  \code{lines(x, y1)} and \code{lines(x, y2)}. If \code{y2} is not supplied
  the area between the curve and the x-axis is drawn. A seen in the examples,
  a neat trick to get the area under the curve is to set 
  \code{y2 = rep(-Inf, length(x))}.
}
\examples{
# Random example 1
x  <- seq(-2*pi, 2*pi, length.out = 100)
y1 <- cos(x)
y2 <- sin(4*x) - x^2 + 10
plot(x, type = "n", ylim = range(c(y1, y2)), xlim = range(x),
     ylab = "y", xlab = "x")
fillLines(x, y1, y2, col = "tomato", border = NA)
fillLines(x, pmin(y1, y2), rep(-Inf, length(x)), 
col = "steelblue", border = NA)
lines(x, y1, lty = 2, lwd = 2)
lines(x, y2, lty = 3, lwd = 2)

# Pursuit curve
x <- seq(0.01, 3, by = 0.01)
y <- 2*x^2 - log(x)
plot(x, y, type = "n", ylim = c(0, max(y)))
fillLines(x, y, density = 10, angle = -30, col = "orange")

# The super egg
p <- 5/2
h <- 1
r <- 3/4*h
x <- seq(-r, r, by = 0.001)
y1 <- h^p*(1 - abs(x/r)^p)^(1/p)
y2 <- -y1
plot(1, type = "n", xlim = c(-r,r), ylim = c(-h, h), asp = 1,
     xlab = "", ylab = "")
fillLines(x, y1, y2, col = "gold", lwd = 2, border = "orange") 
}
\seealso{
\code{\link{polygon}}
}
\author{
Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
}
