mininimum.abs.pcor <- function(X) {
  current.min <- pcor(X, k = 1)
  current.min[is.nan(current.min)] <- 1
  for (k in 2:nrow(X)) {
    new.min <- pcor(X, k = k)
    new.min[is.nan(new.min)] <- 1
    is.new.min <- abs(new.min) < abs(current.min)
    current.min[is.new.min] <- new.min[is.new.min]
  }
  return(current.min)
}
