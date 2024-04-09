#' Set Datum Parent
#'
#' Each element of an XML file has a single parent which is identified by datumEntity. Setting
#' the datumParent attribute preserves the XML structure through transformations.
#'
#' @param x A syntactically correct XML file with datumEntity set.
#'
#' @return A syntactically correct XML file with datumParent set.
#' @export set.datum.parent
#'
#'
set.datum.parent <- function(x) {
  for (i in 1:seq_along(x)) {
    individual.node <- x[i]

    datum.parent <- get.datum.parent(individual.node)

    xml2::xml_attr(individual.node, "datumParent") <- datum.parent

    ##################
    # Does this node have children
    children <- xml2::xml_children(x)
    if (length(children) > 0) {
      # Yes we have children
      for (j in seq_along(children)) {
        child <- children[j]
        set.datum.parent(child)
      }
      #################
    }


    return(x)
  }
}
