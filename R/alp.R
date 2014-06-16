
# Add alpha value to colours
alp <- function (x, alpha) {
  # Adds alpha (transparency) channel to a colour; alpha is either a
  # decimal number between 0 and 1 or a 2-digit hexadecimal number.
  
  # Convert name colours to hex
  hex <- grepl("#", x)
  if (any(!hex)) {
    x[!hex] <- rgb(t(col2rgb(x[!hex]))/255)
  }
  
  # Strip alpha value if present
  if (any(nchar(x) == 9)) {
    x[nchar(x) == 9] <- substr(x[nchar(x) == 9], 1, 7)
  }
  
  if (alpha >= 0 & alpha <= 1) {
    paste0(x, sprintf("%02X", round(255*alpha))) 
  } else if (grepl("[0-9a-fA-F]+", alpha) & nchar(alpha) == 2) {
    paste0(x, alpha)
  } else {
    stop(paste("Alpha is not a decimal number between 0 and 1",
               "or a 2-digit hexadecimal number.\n"))
  }
}

