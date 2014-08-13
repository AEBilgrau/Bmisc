#' Draw two oriented graphs with same layout
#'
#' This function aids in drawing two directed graphNEL objects to have the same
#' node layout but still preserving the edges. This is useful when comparing
#' two graphs.
#'
#' @param g1 A graphNEL object
#' @param g2 A graphNEL object
#' @param col1 Edge color of edges unique to g1
#' @param col2 Edge color of edges unique to g2
#' @param name The name of the graphs.
#' @param \dots Arguments passed to \code{agopen}.
#' @return A \code{list} of length three with the laid out graphs. The first
#'   and second entry is the laid out g1 and g2 with coloured unique edges.
#'   The third element is the merged graph (rarely of particular interest).
#' @details The function works by combining \code{g1} and \code{g2} and making
#'   unique edges of \code{g2} invisible in the merged graph and vice versa
#'   for the unique edges of \code{g1}.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @seealso \code{\link{agopen}}, \code{\link{plot.graphNEL}}
#' @examples
#' library("gRbase")
#' g1 <- dagList(list(~A|B, ~A|C, ~A|D, ~E, ~F|A, ~G))
#' g2 <- dagList(list(~A|B, ~C|A, ~D, ~A|E, ~F|A))
#' cc <- combineAndDraw(g1, g2)
#'
#' col1 <- rep("green", length(edgeNames(g1)))
#' names(col1) <- edgeNames(g1)
#' col2 <- rep("red", length(edgeNames(g2)))
#' names(col2) <- edgeNames(g2)
#'
#' par(oma = c(0,0,0,0)+.2)
#' layout(rbind(1:2,3,4:5))
#' plot(g1, edgeAttrs = list(color = col1), main = "Graph 1"); box()
#' plot(g2, edgeAttrs = list(color = col2), main = "Graph 2"); box()
#' plot(cc[[3]], main = "Merged graph"); box()
#' plot(cc[[1]], main = "Graph 1 (laid out as merged graph)"); box()
#' plot(cc[[2]], main = "Graph 2 (laid out as merged graph)"); box()
#' @export
combineAndDraw <- function(g1, g2,
                           col1 = "green", col2 = "red",
                           name = "",
                           ...) {
  stopifnot(require("igraph"))
  stopifnot(require("Rgraphviz"))
  stopifnot(require("graph"))
  
  gu <- igraph.to.graphNEL(graph.union(igraph.from.graphNEL(g1),
                                       igraph.from.graphNEL(g2),
                                       byname = TRUE))

  test.orientation <- function(u, v) { # test for g1: u -> v && g2: u <- v
    any(graph::edges(g1)[[u]] == v) && 
      any(graph::edges(g2)[[v]] == u)
  }

  test.unique <- function(u, v, g1, g2) {
    # TRUE if edge u -> v or v <- u is in g1 and not g2
    (v %in% graph::edges(g1)[[u]] && !(v %in% graph::edges(g2)[[u]])) |
      (u %in% graph::edges(g1)[[v]] && !(u %in% graph::edges(g2)[[v]]))
  }

  eu1 <- buildEdgeList(gu)
  eu2 <- buildEdgeList(gu)
  eub <- buildEdgeList(gu)

  for (i in 1:length(eub)) {

    u <- from(eub[[i]])
    v <- to(eub[[i]])

    if (eub[[i]]@attrs$dir == "both") {
      # Fix "bi-directed" edges, dir == "both"
      # Managing differing egde orientations
      if (test.orientation(u, v)) {
        # g1: u -> v && g2: u <- v
        eu1[[i]]@attrs$color <- col1
        eu1[[i]]@attrs$dir <- "forward"
        eu2[[i]]@attrs$color <- col2
        eu2[[i]]@attrs$dir <- "back"
      }
      if (test.orientation(v, u)) {
        # g1: u <- v && g2: u -> v
        eu1[[i]]@attrs$color <- col1
        eu1[[i]]@attrs$dir <- "back"
        eu2[[i]]@attrs$color <- col2
        eu2[[i]]@attrs$dir <- "forward"
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

  }

#   for (i in 1:length(nodes)) {
#     nodes[[i]]@attrs$fillcolor <- cols[i]
#     nodes[[i]]@attrs$color <- cols[i]
#     nodes[[i]]@attrs$fontsize  <- 10
#     nodes[[i]]@attrs$shape <- "circle"
#     nodes[[i]]@attrs$fixedsize <- TRUE
#     nodes[[i]]@attrs$height <- 2
#     nodes[[i]]@attrs$width <- 2
#   }

  aggu1 <- agopen(gu, edges = eu1, name = name, ...)
  aggu2 <- agopen(gu, edges = eu2, name = name, ...)
  agguu <- agopen(gu, edges = eub, name = name, ...)

  return(list(aggu1, aggu2, agguu))
}

# str(graph.par())
# str(getDefaultAttrs())

