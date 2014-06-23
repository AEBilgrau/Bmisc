#' @name ListSetOperations
#' @aliases Intersect
#' @aliases Union
#' @aliases Setdiif
#' @title Set operations on lists of character vectors.
#' @details These functions perform basic set operations on lists of character 
#'   vectors in a recursive manner. The work analogous to \code{union}, 
#'   \code{intersect}, and \code{setdiff} but on lists.
#' @param x A \code{list} of \code{character} vectors.
#' @param y A \code{list} of \code{character} vectors.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @seealso \code{\link{union}}, \code{\link{intersect}}, \code{\link{setdiff}}
#' @examples
#' set.seed(1)
#' x <- lapply(2:6, function(i) sample(LETTERS, 10+i))
#' y <- lapply(1:3, function(i) sample(LETTERS, 2+i))
#' 
#' Union(x)  # The union of all vectors in x
#' Intersect(x)  # The intersection of all vectors in x
#' Setdiff(x, y)  # All elements in the intersection of x, but not in any y
NULL