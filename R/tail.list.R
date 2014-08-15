#' @rdname head.list
#' @importFrom utils tail
#' @export
tail.list <- function(x, n = 6L, m = n, ...) {
  stopifnot(length(n) == 1L & length(m) == 1L)
  m <- ifelse(m < 0L, max(length(x) + m, 0L), min(m, length(x)))
  return(lapply(x[seq.int(to = length(x), length.out = m)],
                function(this_x) head(this_x, n = n, ...)))
}
