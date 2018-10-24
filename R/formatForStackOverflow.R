#' @title Run and format R code to StackOverflow submission ready
#' @description Runs the R code, captures both code and output and prints such 
#' that code and output are readily copied into an Stack Overflow post or answer.
#' I.e. it prefixes four spaces and (optinally) comments output.
#' @param user_code A character of length 1 of user code to run and format.
#' @param comment_eval logical. If \code{TRUE} output is prefixed with a comment.
#' @param file A path to a file to write the output to. The default prints to console.
#' @return Prints to console or file a ready-to-copy to Stack Overflow formatted code and output.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @examples
#' formatForStackOverflow("
#'   myfunc <- function(x) {
#'     return(c(mean=mean(x), med=median(x)))
#'   }
#'   
#'   # A comment
#'   n <- 100
#'   myfunc(rnorm(n))
#'   
#'   sapply(mtcars, myfunc) # inline comment
#'   
#'   print(mtcars)
#' ")
#' @export
formatForStackOverflow <- function(user_code, comment_eval = TRUE, file = "") {
  # Capture user code
  if (is.character(user_code)) {
    char_user_code <- user_code
    char_user_code <- unlist(strsplit(char_user_code, "\n"))
  
    # Strip minimal leading spaces
    while (all(substr(char_user_code, 1, 1) %in% c("", " "))) {
      char_user_code <- substring(char_user_code, 2)
    }
    
  } else {
    stop("user_code should be a character format")
    # user_enexpr <- rlang::enexpr(user_code)
    # char_user_code <- as.character(user_enexpr)[-1]
  }

  # Write captured user code to temp file 
  r_file <- tempfile(fileext = ".R")
  writeLines(char_user_code, con = r_file)
  
  # Caputure the sourced output
  out <- capture.output(source(file = r_file, 
                               echo = TRUE, 
                               print.eval = TRUE,
                               prompt.echo = "R>>>",
                               continue.echo = "R>>>",       
                               keep.source = TRUE, 
                               width.cutoff = 1000L))
  unlink(r_file) # File not needed anymore
  
  # Identify lines types
  empty_line <- grepl("^$", out)
  code_line <- startsWith(out, "R>>>")
  eval_line <- !(empty_line | code_line)
  
  # Format to be paste-ready to Stack Overflow
  four_spaces <-  "    "
  out[empty_line] <- four_spaces
  out[code_line] <- gsub("^R>>>", four_spaces, out[code_line])
  out[eval_line] <- paste0(four_spaces, ifelse(comment_eval, "#", ""),
                           out[eval_line])
  
  # Print to console for copying
  cat(out, sep = "\n", file = file)
}






