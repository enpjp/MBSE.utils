#' Set Datum Entity
#'
#' Sets a unique datumEntity value for each element of an XML file.
#'
#' @param x A syntactically correct XML file.
#'
#' @return A syntactically correct XML file with unique datumEntity attributes
#' @export set.datum.entity
#'
#'
set.datum.entity <- function(x) {



  for(i in seq_along(x)) {
    individual.node <- x[i]


    datum.entity <- infer.datum.entity(individual.node)

    xml2::xml_attr(individual.node , "datumEntity") <- datum.entity
  #  xml2::xml_attr(individual.node , "datumAttribute") <- "tag.type"
    xml2::xml_attr(individual.node , "tag.type") <- xml2::xml_name(individual.node)





    # Does this node have children
    children <- xml2::xml_children(x)
    if(length(children) > 0 ) {
      # Yes we have children
      for(j in seq_along(children)) {
        child <- children[j]

        datum.entity <- infer.datum.entity(child )
        xml2::xml_attr(child , "datumEntity") <- datum.entity
        xml2::xml_attr(child , "tag.type") <- xml2::xml_name(child)
        set.datum.entity(child)





      }


    }




 }


  return(x)

}





