#' Triples To Network
#'
#' Converts datum Triples of an XML document into a gglot2 network plot with
#' tag.type as the node and descendent as the edges. This gives some insight
#' into the document structure.
#'
#' @param x Datum Triples of an XML document
#'
#' @return A ggplot2 Network data.frame.
#' @export triples.to.network
#'
#'
triples.to.network <- function(x){

  set.seed(1)

  wide.data.ss <- x  %>% triple.as.wide()

  # Make the vertices (Nodes)

  network.data.vertices <- wide.data.ss %>%
    dplyr::select(.data$datumEntity,
                  data$tag.type)


  # Add a couple of columns for edges

  wide.data.ss$from <- wide.data.ss$datumParent
  wide.data.ss$to <- wide.data.ss$datumEntity

  # The entity at the top of the tree does not have a parent, so is marked base
  # Drop the base link. This would work if we combined multiple diagrams
  network.data.edges<- wide.data.ss %>%
    dplyr::select(.data$from,.data$to) %>%
    dplyr::filter(.data$from != "Base" )

  # Make an network object using `asNetwork` to convert the igraph class into
  # network class. Then `ggnetwork` to the universal dataframe format used in
  # `ggplot2`
  network.graph <- igraph::graph_from_data_frame(
    network.data.edges,
    directed = TRUE,
    vertices = network.data.vertices) %>%
    intergraph::asNetwork() %>%
    ggnetwork::ggnetwork(layout = "fruchtermanreingold")


  return(network.graph)
}



