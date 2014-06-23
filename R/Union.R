#' @rdname ListSetOperations
#' @return \code{Union} returns the union all character vectors in \code{x}.
#' @export
Union <- function (x) {  
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}


