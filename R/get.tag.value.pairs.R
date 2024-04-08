#' Get Tag Value Pairs
#'
#' Expand tag value pairs in the the data into datumAttribute and datumValue.
#'
#' @param x Datum Triple format data.
#'
#' @return datumTriples with tag value pairs.
#' @export get.tag.value.pairs
#'
#'
get.tag.value.pairs <- function(x){
  tag.value.pairs <- x %>% triple.as.wide() %>%
    dplyr::filter( 'tag.type' == "TaggedValue") %>%
    dplyr::select('datumEntity', 'tag', 'value') %>% tidyr::pivot_wider(
      id_cols = 'datumEntity',
      names_from = 'tag',
      values_from = 'value') %>% data.as.triple()

  x.out <- rbind(x, tag.value.pairs )

  return(x.out)

}
