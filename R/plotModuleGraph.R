#' Plot the graphical representation of a weigted adjacency amatrix
#' 
#' A function of for plotting the graphical representation of a weigted
#' weigted adjacencey amatrix. Build for convenience and not for speed.
#' 
#' @param amat A numeric adjacency amatrix.
#' @param diff.exprs A numeric vector of lencth \code{ncol(amat)} giving the 
#'   importance (e.g.\ evidence for differential expression) of the node/gene.
#' @param label The labels of the nodes.
#' @param vcol The colors of the nodes. Default is black.
#' @param \dots Aguments passed to \code{plot.igraph}.
#' @param layout A function providing the layout of the graph.
#'   See \link[igraph]{layout}.
#' @author Anders Ellern Bilgrau <abilgrau (at) amath.aau.dk>
#' @examples
#' # Construct a random adjacency matrix
#' amat <- replicate(10, runif(10))
#' amat[upper.tri(amat)] <- amat[lower.tri(amat)]
#' 
#' # Plot the graph
#' plotModuleGraph(amat,
#'                 label = "",
#'                 vcol = 1:10,
#'                 diff.exprs = 3 + 3*runif(10))
#' @export
plotModuleGraph <- function(amat,
                            diff.exprs = NULL,
                            label = rownames(amat),
                            vcol = "Black",
                            layout = layout.circle,
                            ...) {
  if (!require(igraph)) {
    stop("The igraph package is needed for this function.")
  }
  
  diag(amat) <- NA
  
  # Create full graph
  graph <- graph.adjacency(amat, mode = "undirected",
                           weighted = TRUE,
                           diag = FALSE)
  
  # VERTICES
  V(graph)$color  <- vcol
  V(graph)$weight <- rowSums(amat, na.rm = TRUE)
  V(graph)$size   <- 3
  
  if (!is.null(diff.exprs)) {
    V(graph)$size <- diff.exprs
  }
  
  # EDGES
  tmp <- E(graph)$weight
  E(graph)$color  <- rgb(0,0,0, alpha = tmp)
  E(graph)$width  <- 3*tmp
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
       vertex.label = label,... )
  
}