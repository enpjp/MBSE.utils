#' Turn Data Into Datum Triple format
#'
#' A convenience function for turning data.frames into long datum Triple format
#'
#' @param data data.frame with datumAttribute and named columns with values
#'
#'
#' @return A data.frame of triples datumEntity, datumAttribute, datumValue.
#' @export data.as.triple
#'
data.as.triple <- function(data){

  triple <- data %>%
    purrr::map_df(as.character) %>% # Convert to character
    tidyr::pivot_longer(!'datumEntity', # datumEntity as  key
                 names_to = "datumAttribute", # Attribute name
                 values_to = "datumValue" # Save  value
    ) %>% tidyr::drop_na() # Drop NAs

  return(triple )
}
