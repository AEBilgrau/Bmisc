#' Return the first or last part of a list
#'
#' This function recurses over the first/last elements of a list to return a
#' compact view of the list.
#'
#' @aliases head.list tail.list
#' @param x A list of R objects.
#' @param n A single integer. If positive, size for the resulting object:
#'   number of elements for a vector (including lists), rows for a matrix or
#'   data frame or lines for a function. If negative, all but the n last/first
#'   number of elements of x.
#' @param m As \code{n}. If \code{x} is a list, \code{m} controls the number of
#'   elements from the list to show. Defaults to \code{n}.
#' @param \dots Arguments passed to \code{head}.
#' @return Returns a list of length \code{m} of the first/last element of
#'   \code{x} where the first \code{n} elements of each entry is shown.
#' @seealso \code{\link{head}}
#' @examples
#' x <- list(A = letters[1:9],           B = matrix(runif(30), ncol = 3),
#'           C = as.list(LETTERS[1:10]), D = function(x) { x^2 + 2*x + 1},
#'           D = combn(1:5, m = 4),      E = "A simple string.",
#'           F = data.frame("unif" = runif(10), "norm" = rnorm(10)))
#' str(x)
#' head.list(x, n = 3L)
#' head.list(x, n = 2L, m = 7L)
#' tail.list(x, n = 3L)
#' tail.list(x, n = 3L, m = 3L)
head.list <- function(x, n = 6L, m = n, ...) {
  stopifnot(length(n) == 1L & length(m) == 1L)
  m <- ifelse(m < 0L, max(length(x) + m, 0L), min(m, length(x)))
  return(lapply(x[seq_len(m)],
                function(this_x) head(this_x, n = n, ...)))
}
