#' Any New Nodes
#'
#' @param new.nodes List of nodes
#' @param existing.nodes List of nodes.
#' @param new.node.type Name for any new nodes.
#'
#' @return List of nodes
#' @export any.new.nodes
#'
#'
any.new.nodes <- function(new.nodes, existing.nodes, new.node.type = "Orphan" ) {

  nodes.out <- existing.nodes
  # Now get the nodes
  get.any.new.nodes <- base::setdiff(new.nodes, existing.nodes$datumEntity )

  if(length(get.any.new.nodes) > 0) {

     extra.nodes <- dplyr::tibble( `id` = get.any.new.nodes,
                          `datumEntity` = get.any.new.nodes,
                          `nominal.name` = get.any.new.nodes,
                          `tag.type` = new.node.type )
    # colnames(new.nodes) <- c("datumEntity","tag.type")
    # new.nodes$tag.type <-  "External.node"
    nodes.out <- dplyr::bind_rows( existing.nodes, extra.nodes  )


  }

  return(nodes.out)
}

