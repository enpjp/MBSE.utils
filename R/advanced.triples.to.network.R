
#' Advance Triple Network
#'
#' This function has enhanced checking for missing newtwrk nodes
#'
#' @param x Set of datum triples
#'
#' @return Network object
#' @export advanced.triples.to.network
#'
#'
advanced.triples.to.network <- function(x){

  set.seed(1)

 # wide.data.ss <- x  %>% MBSE.utils::triple.as.wide()

  wide.data.ss <- x  %>% tidyr::pivot_wider(
    id_cols = "datumEntity",
    names_from = "datumAttribute",
 #  values_fn = list,
    values_from = "datumValue"
  ) #%>% mutate_all(as.character())

  # Make the vertices (Nodes)

  network.data.vertices <- wide.data.ss %>%
    dplyr::select(.data$datumEntity,.data$tag.type)


  # Add a couple of columns for edges

  wide.data.ss$from <- wide.data.ss$datumParent
  wide.data.ss$to <- wide.data.ss$datumEntity

  # The entity at the top of the tree does not have a parent, so is marked base
  # Drop the base link. This would work if we combined multiple diagrams

  network.data.edges <- wide.data.ss %>% dplyr::select(.data$from,.data$to) # %>% #filter(from != "Base" )
 # filter(!stringr::str_detect(from,"^BASE")  )

  # Add edge type
  network.data.edges$edge.type <- "XML"

  # Try using regx [A-Z]{2,4}_\S{8,} to find ID patterns.
  # Find rows with ID type pattern
  rows.with.id <- grepl("^[A-Z]{2,4}_\\S{8,}", x$datumValue)
  #attributes.with.id <- x$datumAttribute[rows.with.id]
  #datumEntity.attribues.with.id <- x$datumEntity[rows.with.id]
  triples.with.id <- x[rows.with.id,]
  not.self.loops <- !(triples.with.id$datumEntity == triples.with.id$datumValue)

  triples.with.edges <- triples.with.id[not.self.loops,]

  all.edges <- triples.with.edges %>% dplyr::select(from = .data$datumEntity, to = .data$datumValue) %>% unique()

  all.nodes <- MBSE.utils::any.new.nodes(all.edges$to, network.data.vertices, new.node.type = "Orphan")
  all.nodes <- MBSE.utils::any.new.nodes(all.edges$from,   all.nodes, new.node.type = "Lost.Parent")


    network.graph <- igraph::graph_from_data_frame(
      all.edges,
      directed = TRUE,
      vertices = all.nodes)

      #%>%
      #intergraph::asNetwork()

      #%>%
      #ggnetwork(layout = "fruchtermanreingold")


    return(network.graph)




  # Make an network object using `asNetwork` to convert the igraph class into
  # network class. Then `ggnetwork` to the universal dataframe format used in
  # `ggplot2`


}



