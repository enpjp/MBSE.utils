
#' Add extra attributes to the datumTriples to aid plotting
#'
#' @param triples dataframe of datumTriples
#' @param triples.to.find List of keywords to mark entries.
#' @param datumAttributeCol Name of the attribute column
#' @param datumValueCol Value to use for selected rows
#' @param datumValueNot Value to use for rows not selected
#'
#' @return dataframe of datumTriples
#' @export add.ontology.col
#'
#'
add.ontology.col <- function(triples,
                                   triples.to.find ,
                                   datumAttributeCol = "plot.core",
                                   datumValueCol = "Ontology",
                                   datumValueNot = "XML"){
  # Initialise column
  # triples.new <- triples %>%   filter(!datumValue %in% triples.to.find)
  #
  # triples.new$datumAttribute <- datumAttributeCol
  # triples.new$datumValue <- datumValueNot

  my.triple <-  triples %>%   dplyr::filter(.data$datumValue %in% triples.to.find)
  my.triple$datumAttribute <- datumAttributeCol
  my.triple$datumValue <- datumValueCol
  triples.plus.in <- rbind(triples, my.triple )

  # Now deal with the values not set
  wide.triples.plus.in <- triples.plus.in %>% MBSE.utils::triple.as.wide()
  wide.triples.plus.in[datumAttributeCol][wide.triples.plus.in[datumAttributeCol] != datumValueCol] <- datumValueNot

  triples.out <- wide.triples.plus.in %>% MBSE.utils::data.as.triple()

  return(triples.out)

}
