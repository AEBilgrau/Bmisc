#' Hierarchical Edge Bundling
#' 
#' Visualization of networks using hierarchical edge bundles. 
#' Currently, Bezier curves are used and not B-splines for the bundling. 
#' Hopefully, this will be updated in future versions.
#' 
#' @param phylo A `phylo` object.
#' @param graph A \code{igraph} object.
#' @param beta The amount of bundling.
#' @param include.mrca Should the only the most recent common ancestor or the
#'   full shortest path be used?
#' @param simplify Simplify the paths by taking the convex hull.
#' @param ... Arguments passed to \code{\link[ape]{plot.phylo}}.
#' @param args.lines A list of arguments passed to \code{lines}.
#' @param debug Plot some extra info.
#' @param use.only An integer vector giving the nodes from which edges are
#'   to be drawn. E.g. \code{use.only = 1} will only plot the edges from 
#'   vertex 1.
#' @return Plots to a new device.
#' @seealso See \code{\link[ape]{plot.phylo}}.
#' @author Anders Ellern Bilgrau
#' @references
#'   Holten, Danny. "Hierarchical edge bundles: Visualization of adjacency 
#'   relations in hierarchical data." Visualization and Computer Graphics, IEEE 
#'   Transactions on 12.5 (2006): 741-748.
#' @examples
#' library("igraph")
#' library("ape")
#' n <- 25
#'   
#' # Create graph
#' adj <- abs(cor(matrix(rnorm(n^2), n, n)))
#' rownames(adj) <- colnames(adj) <-
#'   apply(combn(LETTERS, 2), 2, paste0, collapse = "")[1:n]
#' graph <- graph.adjacency(adj, mode = "un", weighted = TRUE, diag = FALSE)
#' E(graph)$weight <- E(graph)$weight^2
#' E(graph)$color <- alp("black", E(graph)$weight/max(E(graph)$weight))
#' 
#' phylo <- as.phylo(hclust(as.dist(1 - adj), method = "ward.D"))
#' 
#' plot(phylo, type = "fan")
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.95,
#'                             args.lines = list(col = alp("steelblue", 90)))
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.75,
#'                             args.lines = list(col = alp("steelblue", 90)))
#'                             
#' par(mfrow = c(1,2))
#' plot(phylo, type = "fan")        
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.99,
#'                             e.use.only = 1,
#'                             debug = TRUE,
#'                             args.lines = list(col = alp("steelblue", 1)))
#' @export
plotHierarchicalEdgeBundles <-
  function(phylo, 
           graph,
           beta = 0.5,
           include.mrca = FALSE,
           simplify = FALSE,
           ...,
           args.lines = list(),
           debug = FALSE,
           v.use.only,
           e.use.only) {
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
    text(pos$x, pos$y, col="black", pos$i, cex = 2)
  }
  
  # Get edgelist (and convert to numeric if names are present)
  es <- get.edgelist(graph)
  if (!is.null(V(graph)$name)) {
    es <- structure(match(es, V(graph)$name), dim = dim(es))
  }
  
  if (!missing(v.use.only)) es <- es[es[,1] %in% v.use.only, , drop = FALSE]
  if (!missing(e.use.only)) es <- es[e.use.only, , drop = FALSE]
  
  # Shortest paths (with start and end) or path through Most Recent Common 
  # Ancestor 
  sp2 <- sp.tips(phylo, es[, 1], es[, 2], include.mrca = include.mrca)
  stopifnot(nrow(es) == length(sp2))
  sp <- lapply(seq_along(sp2), function(i) unname(c(es[i,1],sp2[[i]],es[i,2])))
  names(sp) <- names(sp2)
  
  # Plot spline curve for each path
  for (path in sp) { 
    d <- pos[path, ]
    if (simplify) {
      ch <- chull(d$x, d$y)
      d <- d[match(intersect(d$i, d$i[ch]), d$i), ] # NOT d[match(d$i[ch],d$i),]
    }
    ord <- ifelse(length(d$x) >= 4, 4, length(d$x))
    if (debug) lines(d$x, d$y)
    tmp <- straightenedBSpline(d$x, d$y, order = ord, beta = beta)
    do.call(lines, c(tmp, args.lines))
  }
}
