#' Get XML Attributes
#'
#' A convience function to list all the attributes in an XML file.
#'
#'
#' @param xml.object An XML file
#'
#' @return A list of lists: a sorted list; the number of attributes; a
#'   paragraph.
#' @export get.xml.attrs
#'
#'
get.xml.attrs <- function(xml.object) {

  xml_atts <-  xml.object %>%
    xml2::xml_find_all("//*") %>%
    purrr::map(~names(xml2::xml_attrs(.))) %>%
    unlist() %>%
    unique()

  xml_atts_sorted <- sort(xml_atts)

  number_of_attrs <-  length(xml_atts_sorted)

  all_attributes  <-  knitr::combine_words(xml_atts_sorted)


  output.list <- list(
    "xml_atts_sorted" = xml_atts_sorted,
    "number_of_attrs" = number_of_attrs,
    "all_attributes"  = all_attributes
  )
  return(output.list)

}
