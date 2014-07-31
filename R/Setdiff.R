#' @rdname ListSetOperations
#' @return \code{Setdiff} returns the intersection of \code{x} minus the
#'   union of \code{y}. 
#' @include
#'   Union.R
#'   Intersect.R
#' @seealso \code{\link{without}}
#' @export
Setdiff <- function (x, y) {
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}
