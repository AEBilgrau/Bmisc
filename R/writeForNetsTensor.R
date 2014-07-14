#' Format a list of networks for the "NetsTensor" software.
#'
#' This function formats the networks for the "NetTensor" software by Li et. al.
#' (see references below) and writes the result to the harddrive.
#'
#' @param x.list A list of matrices of the same size
#' @param out.dir character. The path to the output dir where the files are to
#'   be written.
#' @param verbose logical. Should the progress be outputted?
#' @return Returns invisibly a \code{list} of length 4 with the content of the
#'   written files. Writes the formatted networks to
#'   the disk in the directory specified by \code{out.dir}.
#' @references
#'   Li, W., Liu, C. C., Zhang, T., Li, H., Waterman, M. S., & Zhou, X. J.
#'   (2011). Integrative analysis of many weighted co-expression networks using
#'   tensor computation. PLoS computational biology, 7(6), e1001106.
#' @note
#'   The software is available at the website
#'   \url{http://zhoulab.usc.edu/tensor/}.
#' @export
writeForNetsTensor <- function(x.list,
                               out.dir = "netsdata",
                               verbose = TRUE) {
  # Create the output dir
  dir.create(out.dir, showWarnings = FALSE)

  # Function to format the individual networks
  formatForNets <- function (x) {  # x is a numeric matrix
    cbind(get.lower.tri(col(x)),
          get.lower.tri(row(x)),
          get.lower.tri(x))
  }

  # Sorting all matrices as the first matrix and format
  x.list <- lapply(x.list, function(x) x[rownames(x.list[[1]]),
                                         rownames(x.list[[1]])])
  y.list <- lapply(x.list, formatForNets)
  names(y.list) <- paste0("net", seq_along(y.list), ".network")

  # Write the networks
  paths <- file.path(out.dir, names(y.list))
  for (i in seq_along(y.list)) {
    write.table(y.list[[i]], file = paths[i], sep = "\t",
                dec = ".", quote = FALSE,
                col.names = FALSE, row.names = FALSE)
    if (verbose) {
      cat(paths[i], "written.\n")
    }
  }

  # Write datasets_list
  datasets_list <- gsub(".network", "", basename(paths))
  con <- file(file.path(out.dir, "datasets_list"), open = "wb")
  writeLines(datasets_list, file.path(out.dir, "datasets_list"))
  close(con)
  if (verbose) {
    cat(file.path(out.dir, "datasets_list"), "written.\n")
  }

  gsub(".network", "", basename(paths))

  # Write datasets_key
  write.table(cbind(paths, names(x.list)),
              file = file.path(out.dir, "datasets_key"),
              quote = FALSE, col.names = FALSE, row.names = FALSE)

  # Write gene_key
  gene_key <- data.frame(1:nrow(x.list[[1]]), rownames(x.list[[1]]))
  gene_ids <- gene_key[,1]
  write.table(gene_ids, file = file.path(out.dir, "gene_ids"),
              quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(gene_key, file = file.path(out.dir, "gene_key"), sep = "\t",
              quote = FALSE, col.names = FALSE, row.names = FALSE)
  if (verbose) {
    cat(file.path(out.dir, c("gene_ids written\n","gene_key written\n")),sep="")
  }

  # Invisibly return the written objects
  ans <- list("nets" = y.list, datasets_list, gene_ids, gene_key)
  return(invisible(ans))
}
