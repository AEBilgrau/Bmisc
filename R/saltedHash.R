#' Salted hashing function
#'
#' This function adds salt (stochastic or deterministic) to a character
#' of strings prior to a hashing of the complete string.
#' @param salt A character containing the salt
#' @param x A character vector of the strings to be hashed.
#' @param algo The hashing algorithm used. See \code{\link[digest]{digest}} for 
#'   possible values.
#' @param \dots Additional parameters passed to \code{\link[digest]{digest}}.
#' @return A character vector
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @seealso \code{\link[digest]{digest}}
#' @examples
#' x <- c("AVerySecretString", "AnotherSecretSting")
#' saltedHash("SomeSalt", x = x, algo = "md5")
#' #digest(paste0("SomeSalt", "AVerySecretString"), algo = "md5") 
#' @export
saltedHash <- function(salt, x, algo = "sha256", ...) {
  if (!require(digest)) {
    stop("Package digest is need to use the function.")
  }
  z <- y <- paste0(salt, x)  # Add salt
  for (i in seq_along(y)) {  # Hashing
    z[i] <- digest::digest(y[i], algo = algo, ...)
  }
  return(z)
}
