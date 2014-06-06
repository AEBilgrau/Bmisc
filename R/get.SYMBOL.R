# Get Hugo gene symbols and ensembl gene identifiers
# es in an ExpressionSet object
get.SYMBOL <- function(es) {
  res <- 
    getBM(attributes = c(get.attribute(es), "hgnc_symbol", "ensembl_gene_id"),
          filters = get.attribute(es),
          values = featureNames(es),
          mart = ensembl)
  colnames(res) <- c("Probe", "GENE", "ENSG")
  return(res)
}

