#' Convert from bioconductor database array name to biomaRt's array names
#' 
#' @param es is an ExpressionSet object
#' @return A character of length \code{1} with the biomaRt array name
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com>
#' @note Note, not all arrays are implemented. 
#' @seealso \code{\link[biomaRt]{useMart}}, \code{\link[biomaRt]{listMarts}}, 
#'   \code{\link[biomaRt]{useDataset}}, \code{\link[biomaRt]{listDatasets}}
#' @export
get.attribute <- function(es) {
  if (!requireNamespace("biomaRt")) {
    stop("The package biomaRt is required to use the function.")
  }
  array <-
    switch(annotation(es), 
           "hgu133plus2"  = "affy_hg_u133_plus_2",
           "hgu133a"      = "affy_hg_u133a",
           "hgu133b"      = "affy_hg_u133b",
           "hugene10stv1" = "affy_hugene_1_0_st_v1")
  return(array)
}
