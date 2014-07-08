#' The relative complement or set difference
#' 
#' The relative complement of x with respect to y, i.e. x\\y or x minus y.
#' @aliases %w/o%
#' @usage 
#' without(x, y)
#' x \%w/o\% y
#' @param x A character or numeric vector. 
#' @param y A character or numeric vector.
#' @return The functions returns a vector of the elements in x which does not appear in y.
#' @note These functions are identical to \code{\link{setdiff}} but sometimes more easy to use.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' print(x <- LETTERS[1:10])
#' print(y <- LETTERS[5:15])
#' 
#' x %w/o% y
#' without(x, y)
#' setdiff(x, y)
without <- 
  "%w/o%" <- 
    function(x, y) x[!(x %in% y)]

