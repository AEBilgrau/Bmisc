#' Hierarchical Edge Bundling
#' 
#' Visualization of networks using hierarchical edge bundles referenced below. 
#' Plot a graph using a hierchical tree as guides for the edges.
#' This function does not yet (if ever) support the rendering of edges.
#' 
#' @param phylo A \code{phylo} object.
#' @param graph A \code{igraph} object.
#' @param beta The amount of bundling.
#' @param include.mrca Should the only the most recent common ancestor be used
#'   in the shortest path used for the splines?
#' @param simplify Simplify the paths by taking the convex hull of the control
#'   points. Can sometimes yield better results.
#' @param ... Arguments passed to \code{\link[ape]{plot.phylo}}.
#' @param args.lines A list of arguments passed to \code{lines}.
#' @param debug Plot some extra debug info.
#' @param v.use.only An integer vector giving the nodes from which edges are
#'   to be drawn. E.g. \code{use.only = 1} will only plot the edges from 
#'   vertex 1.
#' @param e.use.only An integer vector giving the edges to be drawn. 
#'   E.g. \code{use.only = 1} will only draw the first edge.
#' @return Plots to a new device.
#' @seealso See \code{\link[ape]{plot.phylo}}.
#' @author 
#'   Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
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
#' par(mfrow = c(1,3))
#' plot(phylo, type = "fan")
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.95,
#'                             args.lines = list(col = alp("steelblue", 90)))
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.85,
#'                             args.lines = list(col = alp("steelblue", 90)))
#'              
#' # Extra control of plotting and debugging               
#' par(mfrow = c(1,2))
#' plot(phylo, type = "fan")        
#' plotHierarchicalEdgeBundles(phylo, graph, type = "fan", beta = 0.95,
#'                             v.use.only = 1, debug = FALSE,
#'                             args.lines = list(col = alp("red", 1), lwd = 2))
#' @export
plotHierarchicalEdgeBundles <- function(phylo, 
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
  #stopifnot(require("adephylo"))
  stopifnot(require("ape"))
  
  plot(phylo, edge.color = ifelse(debug,"grey","#00000000"), ...)#, type = "fan")#, ...)
  # Get data
  phy.dat <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  pos <- with(phy.dat, data.frame(i = seq_along(xx), x = xx, y = yy))
  # Add points
  if (debug) {
    points(pos$x, pos$y, col="black", pch = 16, cex = 0.5)
    text(pos$x, pos$y, col="black", pos$i, cex = 1.5)
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
  sp2 <- #sp.tips(phylo, es[, 1], es[, 2], include.mrca = include.mrca)
    sp.tips2(phylo, es[, 1], es[, 2], include.mrca = include.mrca)
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


# MODIFIED sp.tips from adephylo!
sp.tips2 <- function(x, tip1, tip2, useTipNames=FALSE, 
                     quiet=FALSE, include.mrca=TRUE) {
  x <- as(x, "phylo4")
  if (is.character(checkval <- checkPhylo4(x))) 
    stop(checkval)
  t1 <- getNode(x, tip1)
  t2 <- getNode(x, tip2)
  if (any(is.na(c(t1, t2)))) 
    stop("wrong tip specified")
  if (any(c(t1, t2) > nTips(x))) 
    stop("specified nodes are internal nodes")
  if (length(t1) != length(t2)) {
    maxLength <- max(length(t1), length(t2))
    t1 <- rep(t1, length.out = maxLength)
    t2 <- rep(t2, length.out = maxLength)
  }
  toRemove <- (t1 == t2)
  if (sum(toRemove) > 0) {
    t1 <- t1[!toRemove]
    t2 <- t2[!toRemove]
    if (length(t1) == 0) 
      stop("tip1 and tip2 are the same vectors")
    if (!quiet) 
      warning("tip1 and tip2 are sometimes the same; erasing these cases")
  }
  N <- nTips(x)
  root <- getNode(x, N + 1)
  E <- x@edge
  allTips <- unique(c(t1, t2))
  pathTwoTips <- function(path1, path2) {
    cpath <- c(path1, rev(path2))
    temp <- factor(cpath, levels = unique(cpath))
    CA <- temp[table(temp) == 2][1]
    CA <- as.integer(as.character(CA))
    path1 <- path1[1:(which(path1 == CA))]
    temp <- which(path2 == CA)
    if (temp == 1) 
      return(path1)
    path2 <- path2[1:(temp - 1)]
    return(c(path1, path2))
  }
  pathTwoTips.no.mrca <- function(path1, path2) {
    cpath <- c(path1, rev(path2))
    temp <- intersect(path1, path2)
    res <- setdiff(cpath, temp)
    return(res)
  }
  allPathToRoot <- lapply(allTips, function(i) 
    .tipToRoot(x, i, root, include.root = TRUE))
  names(allPathToRoot) <- allTips
  allPath1 <- allPathToRoot[as.character(t1)]
  allPath2 <- allPathToRoot[as.character(t2)]
  if (include.mrca) {
    res <- lapply(1:length(allPath1), function(i) 
      pathTwoTips(allPath1[[i]], allPath2[[i]]))
  }
  else {
    res <- lapply(1:length(allPath1), function(i) 
      pathTwoTips.no.mrca(allPath1[[i]], allPath2[[i]]))
    temp.names <- names(res)
    temp <- sapply(res, function(vec) length(vec) > 0)
    res[temp] <- lapply(res[temp], function(vec) getNode(x, vec))
    names(res) <- temp.names
  }
  if (useTipNames) {
    names(res) <- paste(names(t1), names(t2), sep = "-")
  }
  else {
    names(res) <- paste(t1, t2, sep = "-")
  }
  return(res)
}  
environment(sp.tips2) <- asNamespace("adephylo")


# tip2tip <- function(phylo, t1, t2, include.mrca = FALSE) {
#   x <- as(phylo, "phylo4")
#   #     mrca <- MRCA(x, t1, t2)
#   #     path <-  c(t1, .tipToRoot(x, t1, mrca), 
#   #                if(include.mrca) {mrca}, 
#   #                rev(.tipToRoot(x, t2, mrca)), t2)
#   N <- nTips(x)
#   root <- getNode(x, N + 1)
#   t1 <- getNode(x, tip1)
#   t2 <- getNode(x, tip2)
#   allTips <- unique(c(t1, t2))    
#   allPathToRoot <- lapply(allTips, function(i) 
#     .tipToRoot(x, i, root, include.root =TRUE))
#   names(allPathToRoot) <- allTips
#   allPath1 <- allPathToRoot[as.character(t1)]
#   allPath2 <- allPathToRoot[as.character(t2)]
#   res <- lapply(seq_along(allPath1), function(i) 
#     pathTwoTips(allPath1[[i]], allPath2[[i]]))
# }
# return(paths)


