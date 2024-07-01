#' Turn Data Into Datum Triple format
#'
#' A convenience function for turning data.frames into long datum Triple format.
#' The workhorse of this approach for analysing data.
#'
#' @param x Data.frame with datumAttribute and named columns with values
#'
#'
#' @return A data.frame of triples datumEntity, datumAttribute, datumValue.
#' @export data.as.triple
#'
data.as.triple <- function(x) {
  triple <- x %>%
    purrr::map_df(as.character) %>% # Convert to character
    tidyr::pivot_longer(!.data$datumEntity, # datumEntity as  key
      names_to = "datumAttribute", # Attribute name
      values_to = "datumValue" # Save  value
    ) %>%
    tidyr::drop_na() # Drop NAs

  return(triple)
}
