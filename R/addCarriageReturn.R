# Add a carriage return to txt files
# useful on unix systems to make windows readable txt files
addCarriageReturn <- function(file.in, file.out = file.in) {
  file.in <- path.expand(file.in)
  file.out <- path.expand(file.out)
  if (.Platform$OS.type == "unix") {
    system(paste("sed -e 's/$/\r/' ", file.in, ">", file.out))  
  }
}


