# Get Hugo gene symbols and ensembl gene identifiers
# es in an ExpressionSet object
get.SYMBOL <- function(es, 
                       subset = biomaRt::featureNames(es), 
                       attributes = NULL) {
  if (require(biomaRt)) {
    mart <- biomaRt::useMart("ensembl")
    ensembl <- biomaRt::useDataset("hasapiens_gene_ensembl", mart = mart)
    res <- 
      biomaRt::getBM(attributes = c(get.attribute(es), "hgnc_symbol", 
                                    "ensembl_gene_id", attributes),
                     filters = get.attribute(es),
                     values = subset,
                     mart = ensembl)
    colnames(res) <- c("Probe", "GENE", "ENSG")
    return(res)
  }
}

