# x is a matrix
rowSds <- function(x) {
  n <- ncol(x)
  means <- rowMeans(x)
  return(sqrt(rowMeans((x-means)^2)*(n/(n-1))))
}
