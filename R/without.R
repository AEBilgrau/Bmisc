#' The relative complement or set difference
#' 
#' The page decribes different interfaces for the relative complement of x with 
#' respect to y, i.e. x\\y or x minus y. It is faster than 
#' 
#' @aliases without %w/o% %\\%
#' @usage 
#' without(x, y)
#' x \%w/o\% y
#' @param x A character or numeric vector. 
#' @param y A character or numeric vector.
#' @return The functions returns a vector of the elements in x which does not appear in y.
#' @note These functions are identical to \code{\link{setdiff}} but sometimes more easy to use.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' print(A <- LETTERS[1:10])
#' print(B <- LETTERS[5:15])
#' 
#' A %w/o% B 
#' #A %\% B     # More mathematical notation, single backslash only!
#' without(A, B)
#' setdiff(A, B)
#' 
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark(without(A, B), 
#'                setdiff(A, B))
#' }
#' @export
without <- function(x, y) x[!(x %in% y)]
#' @export 
`%w/o%` <- without
#' @export
`%\\%` <- without
  


