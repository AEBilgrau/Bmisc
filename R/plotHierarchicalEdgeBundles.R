#' Hierarchical Edge Bundling
#' 
#' Visualization of networks using Hierarchical Edge Bundles. 
#' Currently, Bezier curves are used and not B-splines for the bundling. 
#' Hopefully, this will be updated in future versions.
#' 
#' @param phylo A `phylo` object.
#' @param graph A \code{igraph} object.
#' @param beta The amount of bundling.
#' @param use.mrca Should the only the most recent common ancestor or the
#'   full shortest path be used?
#' @param simplify Simplify the paths by taking the convex hull.
#' @param ... Arguments passed to \code{\link[ape]{plot.phylo}}.
#' @param args.lines A list of arguments passed to \code{lines}.
#' @param debug Plot some extra info.
#' @return Plots to a new device.
#' @seealso See \code{\link[ape]{plot.phylo}}.
#' @author Anders Ellern Bilgrau
#' @references
#'   Holten, Danny. "Hierarchical edge bundles: Visualization of adjacency 
#'   relations in hierarchical data." Visualization and Computer Graphics, IEEE 
#'   Transactions on 12.5 (2006): 741-748.
#' @examples
#' if (require("igraph")) {
#'   graph <- erdos.renyi.game(n = 200, p = 0.05)
#'   wt <- walktrap.community(graph, modularity=TRUE)
#'   phylo <- asPhylo(wt)
#'   plotHierarchicalEdgeBundles(phylo, 
#'                               graph,
#'                               beta = 0.2,
#'                               use.mrca = FALSE,
#'                               type = "unrooted", 
#'                               use.edge.length = TRUE,
#'                               simplify = FALSE,
#'                               lab4ut = "axial",
#'                               args.lines = list(col = "#FF000060"))
#' }
#' @export

plotHierarchicalEdgeBundles <-
  function(phylo, 
           graph,
           beta = 0.5,
           use.mrca = TRUE,
           simplify = FALSE,
           ...,
           args.lines = list(),
           debug = FALSE) {
  stopifnot(require("igraph"))
  stopifnot(require("adephylo"))
  stopifnot(require("ape"))
    
  plot(phylo, edge.color = "#00000000", ...)
  # Get data
  phy.dat <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  pos <- with(phy.dat, data.frame(i = seq_along(xx), x = xx, y = yy))
  # Add points
  if (debug) {
    points(pos$x, pos$y, col="black", pch = 16, cex = 0.5)
    text(pos$x, pos$y, col="black", pos$i, cex = 0.5)
  }
  
  es <- get.edgelist(graph)
  
  # Shortest paths (with start and end) or path through Most Recent Common 
  # Ancestor
  if (use.mrca) {
    sp <- lapply(seq_len(nrow(es)), function(i) {
      c(es[i,1], getMRCA(phylo, es[i,]), es[i,2])})
  } else {
    sp <- sp.tips(phylo, es[, 1], es[, 2])
    stopifnot(nrow(es) == length(sp))
    sp <- lapply(seq_along(sp), function(i) 
      unname(c(es[i, 1], sp[[i]], es[i, 2])))
  }
  for (path in sp) { # plot bezier curve each path
    d <- pos[path, ]
    if (simplify) {
      ch <- chull(d$x, d$y)
      d <- d[match(intersect(d$i, d$i[ch]), d$i), ] # NOT d[match(d$i[ch], d$i),]
    }
    #if (debug) { points(d$x, d$y) }
    do.call(lines, c(straightenedBezier(d$x, d$y, beta = beta), args.lines))
  }
  
}

#' Straightened Bezier curves
#' 
#' Function for straightning Bezier curves.
#' 
#' @param x Some x-values
#' @param y Some y-values (optional)
#' @param beta A numeric values that control amount of straightening.
#'   A \code{beta} of \eqn{1} is a normal Bezier curve and where as a value of
#'   \eqn{0} is a straight line.
#' @param evaluation The number of evaluations of the Bezier curve.
#' @return Returns the 
#' @author Anders Ellern Bilgrau
#' @keywords internal
straightenedBezier <- function(x, y, beta = 0.5, evaluation = 100) {

  stopifnot(require("Hmisc"))
  if (missing(y)) {
    y <- x[[2]]
    x <- x[[1]]
  }
  n <- length(x)
  
  # Straigten by control points
  sequ <- (seq_len(n) - 1)/(n - 1)
  x <- beta*x + (1 - beta)*( x[1] + sequ*(x[n] - x[1]) )
  y <- beta*y + (1 - beta)*( y[1] + sequ*(y[n] - y[1]) )
  
  # Construct and return Bezier
  return(bezier(x, y, evaluation = evaluation))
}

