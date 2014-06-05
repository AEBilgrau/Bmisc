# Remove the union of the y's from the common x's. 
# x and y are lists of characters.
Setdiff <- function (x, y) {
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}
