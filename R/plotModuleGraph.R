#' Plot the graphical representation of a weighted adjacency amatrix
#'
#' A function of for plotting the graphical representation of a weigted
#' weighted adjacencey amatrix. Build for convenience and not for speed.
#'
#' @param amat A numeric adjacency a matrix with values between 0 and 1.
#' @param diff.exprs A numeric vector of lencth \code{ncol(amat)} giving the
#'   importance (e.g.\ evidence for differential expression) of the node/gene.
#' @param labels The labels of the nodes.
#' @param vcol The colors of the nodes. Default is black.
#' @param ecol The colour of the edges. Default is black.
#' @param layout A function providing the layout of the graph.
#'   See \link[igraph]{layout}.
#' @param \dots Aguments passed to \code{plot.igraph}.
#' @return Invisibly returns the igraph-object
#' @author Anders Ellern Bilgrau <abilgrau (at) amath.aau.dk>
#' @examples
#' # Construct a random adjacency matrix
#' amat <- replicate(10, runif(10))
#' amat[upper.tri(amat)] <- amat[lower.tri(amat)]
#'
#' # Plot the graph
#' plotModuleGraph(amat,
#'                 labels = "",
#'                 vcol = 1:10,
#'                 diff.exprs = 3 + 3*runif(10))
#' @export
plotModuleGraph <- function(amat,
                            diff.exprs = NULL,
                            labels = rownames(amat),
                            vcol = "Black",
                            ecol = "Black",
                            layout = layout.circle,
                            ...) {
  if (!require(igraph)) {
    stop("The igraph package is needed for this function.")
  }

  # Create full graph
  graph <- graph.adjacency(amat, mode = "undirected",
                           weighted = TRUE,
                           diag = FALSE)


  # VERTICES
  rs <- rowSums(amat, na.rm = TRUE)
  V(graph)$color  <- vcol
  V(graph)$size   <- (rs - min(rs))/sd(rs)

  if (!is.null(diff.exprs)) {
    V(graph)$size <- diff.exprs
  }

  # EDGES
  tmp <- -log(-log(E(graph)$weight))
  tmp <- tmp - min(tmp)  # Translate
  tmp <- tmp/max(tmp)    # Scale
  E(graph)$color  <- alp(ecol, alpha = tmp)
  E(graph)$width  <- tmp
  E(graph)$curved <- 0.0

  # Layout
  #l <- layout.fruchterman.reingold(graph, niter = 4000,
  #                                 weights = E(graph)$weight)
  l <- layout(graph)

  plot(graph, layout = l,
       vertex.frame.color = NA,
       vertex.label.font = 3,
       vertex.label.cex = 0.8,
       vertex.label.color = "black",
       vertex.label.dist = 0.0,
       vertex.label = labels, ...)

  return(invisible(graph))
}
