#' Add or remove \code{_at} from string
#' 
#' Adds (\code{add_at}) or removes (\code{add_at}) the string \code{"_at"} 
#' suffix from a character vector. The \code{"_at"} is often added by various  
#' bioconductor microarray preprocessing packages.
#' 
#' @param x A character vector of feature name
#' @return \code{add_at} and \code{rm_at} both return a character of the same
#'   length as \code{x} with and without \code{"_at"}, respectively.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @examples
#' add_at(c("hello", "world", "with_at"))
#' @export
add_at <- function(x) {  # Add _at on character vector if not present.
  return(paste0(x, ifelse(grepl("_at$", x), "", "_at")))
}

#' @rdname add_at
#' @examples
#' rm_at(c("hello_at", "world_at", "nc"))
#' @export
rm_at <- function(x) {  # Remove _at from character vector if present.
  return(gsub("_at$", "", x))
}
