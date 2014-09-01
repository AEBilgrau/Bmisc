#' Draw two oriented graphs with same layout
#'
#' This function aids in drawing two directed graphNEL objects to have the same
#' node layout but still preserving the edges. This is useful when comparing
#' two graphs. The function also highlights the differences in the edges.
#'
#' @param g1 A graphNEL object
#' @param g2 A graphNEL object
#' @param col1 Edge color of edges unique to g1
#' @param col2 Edge color of edges unique to g2
#' @param cols Node colors. A named character vector of colors where the names
#'   are the node labels. If the length is 1 the color is reused.
#' @param size A vector of node sizes. If the length is 1 the size is reused.
#' @param fontsize A vector of the sizes of the node labels. If the length is 1
#'   the given size is reused.
#' @param name The name of the graphs.
#' @param \dots Arguments passed to \code{agopen}.
#' @return A \code{list} of length three with the laid out graphs. The first
#'   and second entry is the laid out g1 and g2 with coloured unique edges.
#'   The third element is the merged graph (rarely of particular interest).
#' @details The function works by combining \code{g1} and \code{g2} and making
#'   unique edges of \code{g2} invisible in the merged graph and vice versa
#'   for the unique edges of \code{g1}.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @seealso
#'   \code{\link[Rgraphviz]{agopen}},
#'   \code{\link[Rgraphviz]{plot.graphNEL}}
#' @examples
#' library("gRbase")
#' library("Rgraphviz")
#' g1 <- dagList(list(~A|B, ~A|C, ~A|D, ~E, ~F|A, ~G, ~H|I, ~I|H,
#'                    ~J|K, ~K|J, ~L|M, ~X|Y, ~Y|X))
#' g2 <- dagList(list(~A|B, ~C|A, ~D, ~A|E, ~F|A, ~I|H,
#'                    ~J|K, ~K|J, ~L|M, ~M|L))
#' cc <- combineAndDraw(g1, g2)
#'
#' col1 <- rep("steelblue", length(edgeNames(g1)))
#' names(col1) <- edgeNames(g1)
#' col2 <- rep("tomato", length(edgeNames(g2)))
#' names(col2) <- edgeNames(g2)
#'
#' par(oma = c(0,0,0,0)+.4)
#' layout(rbind(1:2,c(3,6),4:5))
#' plot(g1, edgeAttrs = list(color = col1), main = "Graph 1"); box()
#' plot(g2, edgeAttrs = list(color = col2), main = "Graph 2"); box()
#' plot(cc[[3]], main = "Merged graph"); box()
#' plot(cc[[1]], main = "Graph 1 (laid out as merged graph)"); box()
#' plot(cc[[2]], main = "Graph 2 (laid out as merged graph)"); box()
#' @export
combineAndDraw <- function(g1, g2,
                           col1 = "steelblue",
                           col2 = "tomato",
                           cols,
                           size,
                           fontsize,
                           name = "",
                           ...) {
  stopifnot(require("igraph"))
  stopifnot(require("Rgraphviz"))
  stopifnot(require("graph"))
  
  # Make sure both graphs are directed
  g1@graphData$edgemode <- "directed"
  g2@graphData$edgemode <- "directed"
  
  # Combine
  gu <- igraph.to.graphNEL(graph.union(igraph.from.graphNEL(g1),
                                       igraph.from.graphNEL(g2),
                                       byname = TRUE))
  edgePresent <- function (u, v, g) {
    # True if the edge u -> v is in g
    v %in% graph::edges(g)[[u]]
  }

  test.orientation <- function(u, v, g1, g2) {
    # Test that u -> v in g1 and u <- v in g2
    edgePresent(u, v, g1) && edgePresent(v, u, g2)
  }

  test.unique <- function(u, v, g1, g2) {
    # Test that u -> v or v <- u is in g1 and not g2
    (edgePresent(u, v, g1) && !edgePresent(u, v, g2)) |
      (edgePresent(v, u, g1) && !edgePresent(v, u, g2))
  }

  eu1 <- buildEdgeList(gu)
  eu2 <- buildEdgeList(gu)
  eub <- buildEdgeList(gu)


  for (i in seq_along(eub)) {

    u <- from(eub[[i]])
    v <- to(eub[[i]])

    if (eub[[i]]@attrs$dir == "both") {
      # Fix "bi-directed" edges, dir == "both"
      # Managing differing egde orientations
      
      if (test.orientation(u, v, g1, g2)) {
        # If g1: u -> v && g2: u <- v
        eu1[[i]]@attrs$color <- col1
        eu1[[i]]@attrs$dir <- "forward"
        eu2[[i]]@attrs$color <- col2
        eu2[[i]]@attrs$dir <- "back"
      }
      if (test.orientation(v, u, g1, g2)) {
        # If g1: u <- v && g2: u -> v
        eu1[[i]]@attrs$color <- col1
        eu1[[i]]@attrs$dir <- "back"
        eu2[[i]]@attrs$color <- col2
        eu2[[i]]@attrs$dir <- "forward"
      }

      if (edgePresent(u, v, g1) && edgePresent(v, u, g1)) {
        eu1[[i]]@attrs$dir <- "none"
      }

      if (edgePresent(u, v, g2) && edgePresent(v, u, g2)) {
        eu2[[i]]@attrs$dir <- "none"
      }

      if (edgePresent(u, v, g1) && edgePresent(v, u, g1) &&
          edgePresent(u, v, g2) && edgePresent(v, u, g2)) {
        eu1[[i]]@attrs$color <- "Black"
        eu2[[i]]@attrs$color <- "Black"
      }
      
      # Handle double edges present g1 but not in g2
      if (!edgePresent(u, v, g2) && !edgePresent(v, u, g2)) {
        eu2[[i]]@attrs$dir <- "none"
        eu2[[i]]@attrs$color <-"#00000000"
      }

      if (!edgePresent(u, v, g1) && !edgePresent(v, u, g1)) {
        eu1[[i]]@attrs$dir <- "none"
        eu1[[i]]@attrs$color <-"#00000000"
      }
      
    } else {

      # If not a bi-directed edges in the merged graph
      # Handle/colour unique edges
      if (test.unique(u, v, g1, g2)) {
        eu1[[i]]@attrs$color <- col1
        eu2[[i]]@attrs$color <-"#00000000"
      }
      if (test.unique(u, v, g2, g1)) {
        eu1[[i]]@attrs$color <-"#00000000"
        eu2[[i]]@attrs$color <- col2
      }
    }

  }  ## End of for-loop

  nodes <- buildNodeList(gu)

  if (!missing(fontsize) && length(fontsize) == 1) {
    fontsize <- rep(fontsize, length(nodes))
  }
  if (!missing(size) && length(size) == 1) {
    size <- rep(size, length(nodes))
  }
  if (!missing(cols) && length(cols) == 1) {
    cols <- rep(cols, length(nodes))
  }
  for (i in seq_along(nodes)) {
    if (!missing(cols)) {    stopifnot(names(nodes)[i] == names(cols)[i])
                             nodes[[i]]@attrs$fillcolor <- cols[i]
                             nodes[[i]]@attrs$color     <- cols[i]}
    if (!missing(fontsize))  nodes[[i]]@attrs$fontsize  <- fontsize[i]
    if (!missing(size))      nodes[[i]]@attrs$height    <- size[i]
    if (!missing(size))      nodes[[i]]@attrs$width     <- size[i]
    nodes[[i]]@attrs$shape     <- "circle"
    nodes[[i]]@attrs$fixedsize <- FALSE  
  }

  aggu1 <- agopen(gu, edges = eu1, nodes = nodes, name = name, ...)
  aggu2 <- agopen(gu, edges = eu2, nodes = nodes, name = name, ...)
  agguu <- agopen(gu, edges = eub, name = name, ...)

  return(list(aggu1, aggu2, agguu))
}

# str(graph.par())
# str(getDefaultAttrs())