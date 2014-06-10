# Function for appending objects to an existing .RData file
# I used modified version of a function created by user "flodel":
# http://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file
resave <- function(..., list = character(), file) {
  if (!file.exists(file)) { # If file does not exists resave functions as save
    save(..., list = list, file = file)
  }
  previous  <- load(file)  # Returns the loaded object names
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) {
    assign(var, get(var, envir = parent.frame()))
  }
  save(list = unique(c(previous, var.names)), file = file)
}
