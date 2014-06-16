#' Add transparency to colours
#'
#' The function adds transparency to a character of colours names or 
#' hexadecimal values.
#' @param A character of colours. Either as names (e.g. \code{"blue"}) or 
#'  hexadecimal (\code{"#00FF00"}). 
#' @param A numeric value with the alpha level. If \code{alpha = 0} is 
#'  completely transparent and  \code{alpha = 1} is opaque.
#' @return A character of hexadecimal values with alpha value added.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @examples
#' x <- c("Blue", "#402000", NA, "Tomato", "#00FF0250")
#' alp(x, alpha = 0.5)
alp <- function (x, alpha) {
  # Add alpha value to colours
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

