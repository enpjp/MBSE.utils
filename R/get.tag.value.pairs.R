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
get.tag.value.pairs <- function(x) {

  wide.x <- x %>% triple.as.wide()

  tag.present <- "TaggedValue" %in% colnames( wide.x)

  if( tag.present) {
    tag.value.pairs <- x %>%
      triple.as.wide() %>%
      dplyr::filter(.data$tag.type == "TaggedValue") %>%
      dplyr::select(.data$datumEntity, .data$tag, .data$value) %>%
      tidyr::pivot_wider(
        id_cols = .data$datumEntity,
        names_from = .data$tag,
        values_from = .data$value
      ) %>%
      data.as.triple()

    x.out <- rbind(x, tag.value.pairs)

  }else{
    x.out <- x

  }



  return(x.out)
}
