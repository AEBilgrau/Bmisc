#' Get Hugo gene symbols and ensembl gene identifiers
#' 
#' These functions uses biomaRt to get gene symbols and ensembl gene identifies 
#' for an ExpressionSet object.
#' 
#' @param es in an ExpressionSet object
#' @param subset is a character vector of feature names.
#' @param attributes other attributes to extract. A possible list of attributes
#'   can be retrieved using the function \code{listAttributes}.
#' @param \dots arguments passed to \code{getBM}.
#' @return  a \code{data.frame} with columns corresponding to the matched gene identifiers.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @export
get.SYMBOL <- function(es,
                       subset = featureNames(es), 
                       attributes = NULL) {
  if (require(biomaRt)) {
    mart <- useMart("ensembl")
    ensembl <- useDataset("hasapiens_gene_ensembl", mart = mart)
    res <- 
      getBM(attributes = c(get.attribute(es), "hgnc_symbol", 
                           "ensembl_gene_id", attributes),
            filters = get.attribute(es),
            values = subset,
            mart = ensembl, ...)
    colnames(res) <- c("Probe", "GENE", "ENSG")
    return(res)
  }
}

