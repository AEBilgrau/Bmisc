#' List objects and their memory usage
#'
#' This functions lists the object names, type, size, and number of rows and
#' columns in the specificed enviroment. \code{lsos()} is a shorthand interface
#' to list the most memory heavy objects.
#'
#' @rdname ls.objects
#' @aliases ls.objects .ls.objects lsos
#' @param pos Specifies the environment as a position in the search list.
#' @param pattern  an optional regular expression. Only names matching pattern
#'   are returned.
#' @param order.by A character giving the column-name to sort by. Can be one of
#'   \code{"Type"}, \code{"Size"}, \code{"Rows"}, or
#'   \code{"Columns"}.
#'   If not supplied, the \code{data.frame} is not ordered.
#' @param decreasing logical. Should the rows be in decreasing order?
#' @param head logical. Should only the rows be returned?
#' @param n An integer giving the number of rows to be printed. Only used in
#'   \code{head} is \code{TRUE}.
#' @return A \code{data.frame} with columns \code{"Type"}, \code{"Size"},
#'   \code{"Rows"}, \code{"PrettySize"}, and \code{"Columns"}.
#' @author Based on Dirk Eddelbuettel, Petr Pikal, David Hinds, Tony Breyal,
#'   JD Long
#' @references From
#'   \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
#' @examples
#' A <- 1
#' B <- cbind(1:2, 2:3)
#' C <- replicate(10, rnorm(1e6))
#' Bmisc:::.ls.objects()
#' Bmisc:::.ls.objects(order.by = "Size")
#' lsos()
#' @importFrom utils object.size
.ls.objects <- function(pos = 1L,
                        pattern,
                        order.by,
                        decreasing = FALSE,
                        head = FALSE,
                        n = 5) {
  napply <- function(names, fn) {
    sapply(names, function(x) fn(get(x, pos = pos)))
  }
  names <- ls(pos = pos, pattern = pattern)
  if (length(names) == 0L) {
    return(data.frame())
  }
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode  <- napply(names, mode)
  obj.type  <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size  <- napply(names, object.size)
  obj.prettysize <-
    napply(names, function(x) format(object.size(x), units = "auto"))
  obj.dim   <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by)) {
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- head(out, n)
  }
  return(out)
}

#' @rdname ls.objects
#' @param \dots Parameters assed to \code{ls.objects}.
#' @export
lsos <- function(..., n = 10) { # Shorthand usage of .ls.objects
  .ls.objects(..., order.by="Size", decreasing = TRUE, head = TRUE, n = n)
}
