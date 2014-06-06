# Without and %w/o% functions
# All x which does not appear in y
# Identical in output to setdiff function
"%w/o%" <- without <- function(x, y) x[!(x %in% y)]

