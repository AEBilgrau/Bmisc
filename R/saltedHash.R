saltedHash <- function(salt, x, algo = "sha256", ...) {
  if (require(digest))
    stop("Package digest is need to use the function.")
  z <- y <- paste0(salt, x)  # Add salt
  for (i in seq_along(y)) {  # Hashing
    z[i] <- digest(y[i], algo = algo, ...)
  }
  return(z)
}