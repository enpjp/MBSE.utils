#' Datum Entity Parent
#'
#' Infers the datumEntity of the parent node. Used to help function set.datum.parent
#'
#' @param x A syntactically correct XML file.
#'
#' @return A syntactically correct XML file with datumParent set.
#'
#'
#'
get.datum.parent <- function(x) {
  # logical test
  generate.hash <- rlang::hash(Sys.time())
  datum.entity.parent <-  paste("BASE", generate.hash, sep = "_" )

  # has.xmi.id <-   xml2::xml_has_attr(x, 'xmi.id')
  #
  # if( has.xmi.id){
  #
  #   datum.entity.value <- xml2::xml_attr(x, 'xmi.id')
  # }else{
  x.parent <- xml2::xml_parent(x)
  has.datumEntity <- xml2::xml_has_attr(x.parent, "datumEntity")

  if (has.datumEntity) {
    datum.entity.parent <- xml2::xml_attr(x.parent, "datumEntity")
  }

  # datum.entity.parent <- xml2::xml_attr(x, 'datumEntity')



  return(datum.entity.parent)
}
