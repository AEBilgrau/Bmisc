#' Hierarchical Edge Bundling
#' 
#' Visualization of networks using Hierarchical Edge Bundles. 
#' Currently, Bezier curves are used and not B-splines for the bundling. 
#' Hopefully, this will be updated in future versions.
#' 
#' @param phylo A `phylo` object.
#' @param graph A \code{igraph} object.
#' @param beta The amount of bundling.
#' @param use.only.mrca Should the only the most recent common ancestor or the
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
#'   n <- 100
#'   graph <- erdos.renyi.game(n = n, p = 0.02)
#'   V(graph)$name <- apply(combn(LETTERS, 2), 2, paste0, collapse = "")[1:n]
#'   wt <- walktrap.community(graph, modularity=TRUE)
#'   phylo <- asPhylo(wt)
#'   plotHierarchicalEdgeBundles(phylo, 
#'                               graph,
#'                               beta = 0.2,
#'                               use.only.mrca = FALSE,
#'                               type = "fan", #or "unrooted" 
#'                               use.edge.length = TRUE,
#'                               simplify = FALSE,
#'                               lab4ut = "axial",
#'                               args.lines = list(col = "#FF00FFFF"))
#' }
#' @export
plotHierarchicalEdgeBundles <-
  function(phylo, 
           graph,
           beta = 0.5,
           use.only.mrca = TRUE,
           simplify = FALSE,
           ...,
           args.lines = list(),
           debug = FALSE) {
  stopifnot(require("igraph"))
  stopifnot(require("adephylo"))
  stopifnot(require("ape"))
    
  plot(phylo, edge.color = "#00000000", ...)#, type = "fan")#, ...)
  # Get data
  phy.dat <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  pos <- with(phy.dat, data.frame(i = seq_along(xx), x = xx, y = yy))
  # Add points
  if (debug) {
    points(pos$x, pos$y, col="black", pch = 16, cex = 0.5)
    text(pos$x, pos$y, col="black", pos$i, cex = 0.5)
  }
  
  es <- get.edgelist(graph)
  if (!is.null(V(graph)$name)) {
    es <- structure(match(es, V(graph)$name), dim = dim(es))
  }
  
  # Shortest paths (with start and end) or path through Most Recent Common 
  # Ancestor
  if (use.only.mrca) {
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
