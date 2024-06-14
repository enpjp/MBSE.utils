#' Triples To Context Network
#'
#'Converts triples into a network.
#'
#' @param x Set of datum triples
#' @param cols.to.include List of columns to include.
#' @param drop.xml.edges Logical. Should we drop the xml edges
#' @param correct.dup.atts combine duplicate attributes
#'
#' @return Network object
#' @export triples.to.context.network
#'
triples.to.context.network <- function(x,
                  cols.to.include = c("id", "datumEntity","tag.type", "nominal.name"),
                  drop.xml.edges = TRUE,
                  correct.dup.atts = TRUE){

  set.seed(1)

 # wide.data.ss <- x  %>% MBSE.utils::triple.as.wide()

  if(correct.dup.atts) {
    wide.data.ss <- x  %>% tidyr::pivot_wider(
      id_cols = "datumEntity",
      names_from = "datumAttribute",
        values_fn = list,
      values_from = "datumValue"
    ) #%>% mutate_all(as.character())
    wide.data.ss[] <- lapply( wide.data.ss, paste0)

  }else{
    wide.data.ss <- x  %>% tidyr::pivot_wider(
      id_cols = "datumEntity",
      names_from = "datumAttribute",
      #  values_fn = list,
      values_from = "datumValue"
    ) #%>% mutate_all(as.character())
    # wide.dup.ss[] <- lapply( wide.dup.ss, paste0)


  }







  # Correct the ID column with the unique IDs
  wide.data.ss$id <-  wide.data.ss$datumEntity



  network.data.vertices <- wide.data.ss  # %>% select( all_of(cols.to.include) )
  # Are there any duplicate datumEntitiescols.to.include
#  dup.datumEntity <- network.data.vertices %>% duplicated() %>% which()

  # Nope

  # Add a couple of columns for edges

  wide.data.ss$from <- wide.data.ss$datumParent
  wide.data.ss$to <- wide.data.ss$datumEntity

  # The entity at the top of the tree does not have a parent, so is marked base
  # Drop the base link. This would work if we combined multiple diagrams

  network.data.edges <- wide.data.ss %>% dplyr::select(.data$from,.data$to) # %>% #filter(from != "Base" )
 # filter(!stringr::str_detect(from,"^BASE")  )

  # Add edge type
  network.data.edges$edge.type <- "XML"
  # Not sure that we need the above lines

  # Try using regx [A-Z]{2,4}_\S{8,} to find ID patterns.
  # Find rows with ID type pattern
  rows.with.id <- grepl("^[A-Z]{4,4}_\\S{8,}", x$datumValue)
  #attributes.with.id <- x$datumAttribute[rows.with.id]
  #datumEntity.attribues.with.id <- x$datumEntity[rows.with.id]
  triples.with.id <- x[rows.with.id,]
  not.self.loops <- !(triples.with.id$datumEntity == triples.with.id$datumValue)

  triples.with.edges <- triples.with.id[not.self.loops,]

  # Drop edges we do not want
  if(drop.xml.edges){
    # Drop datumParent
    triples.with.edges.not.parents <- triples.with.edges %>% dplyr::filter(stringr::str_detect(.data$datumAttribute,"^datumParent", negate = TRUE)  )
    # triples.with.edges.not.parents <- triples.with.edges.not.parents %>% filter(!stringr::str_detect(datumAttribute,"^dup.id")  )
    all.edges <- triples.with.edges.not.parents %>% dplyr::select(from = .data$datumEntity, to = .data$datumValue) %>% unique()
  }else{

    all.edges <- triples.with.edges %>% dplyr::select(from = .data$datumEntity, to = .data$datumValue) %>% unique()
  }



  #######################
  # Need better Logic to generate an annotated node list.
  #######################



  all.nodes <- any.new.nodes(all.edges$to, network.data.vertices, new.node.type = "Orphan")
  all.nodes <- any.new.nodes(all.edges$from,   all.nodes, new.node.type = "Lost.Parent")

  # Make a nominal.name column
  all.nodes$nominal.name <- all.nodes$name
  list.the.rows.with.NA <- all.nodes$name %>% is.na() %>% which()
  # Overwrite NA rows with tag.type
  all.nodes$nominal.name[list.the.rows.with.NA] <- all.nodes$tag.type[list.the.rows.with.NA]
  list.the.rows.with.NULL <-  grepl("NULL", all.nodes$name ) %>% which()
  # Overwrite NULL rows with tag.type
  all.nodes$nominal.name[list.the.rows.with.NULL] <- all.nodes$tag.type[list.the.rows.with.NULL]

  # Make the vertices (Nodes)

  all.nodes <- all.nodes %>% dplyr::select( dplyr::all_of(cols.to.include) )

  ######################
  ######################

  # Are there any duplicates nodes?
  dup.all.nodes <- all.nodes$datumEntity %>% duplicated() %>% which()


    network.graph <- igraph::graph_from_data_frame(
      all.edges,
      directed = FALSE,
      vertices = all.nodes)

      #%>%
      #intergraph::asNetwork()

      #%>%
      #ggnetwork(layout = "fruchtermanreingold")

    isolated.g <- which(igraph::degree(network.graph)==0)
    cleaned.g = igraph::delete_vertices(network.graph, isolated.g )

    return(cleaned.g)




  # Make an network object using `asNetwork` to convert the igraph class into
  # network class. Then `ggnetwork` to the universal dataframe format used in
  # `ggplot2`


}



