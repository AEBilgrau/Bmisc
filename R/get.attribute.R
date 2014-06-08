# Convert from bioconductor array name to biomaRt's array names
# es is an ExpressionSet object
get.attribute <- function(es) {
  array <-
    switch(annotation(es), 
           "hgu133plus2"  = "affy_hg_u133_plus_2",
           "hgu133a"      = "affy_hg_u133a",
           "hgu133b"      = "affy_hg_u133b",
           "hugene10stv1" = "affy_hugene_1_0_st_v1")
  return(array)
}
