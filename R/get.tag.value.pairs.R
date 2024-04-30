#' Get Tag Value Pairs
#'
#' Expand tag value pairs in the the data into datumAttribute and datumValue.
#'
#' @param drop.tag.cols Logical. If TRUE drop the columns 'tag' and 'value'.
#' @param x Datum Triple format data.
#'
#' @return datumTriples with tag value pairs.
#' @export get.tag.value.pairs
#'
#'
get.tag.value.pairs <- function(x, drop.tag.cols = TRUE) {

  wide.x <- x %>% triple.as.wide()

  tag.present <- "TaggedValue" %in% wide.x$tag.type

  if( tag.present) {
    # Find rows with "TaggedValue"
    rows.with.taggedvalue <- which(wide.x$tag.type == "TaggedValue"  )
    taggedvalue.df <- wide.x[rows.with.taggedvalue,]
    taggedvalue.df <- taggedvalue.df %>%
      dplyr::select(.data$datumEntity, .data$tag, .data$value)

    expanded.df <- taggedvalue.df   %>%
      tidyr::pivot_wider(
        id_cols = .data$datumEntity,
        names_from = .data$tag,
        values_from = .data$value
      )
    tagged.out <-   expanded.df  %>% data.as.triple()
    # Now drop null values
    tagged.out <-  tagged.out[!(is.na(tagged.out$datumValue) | tagged.out$datumValue=="NULL"), ]

    x.out <- rbind(x, tagged.out)

  }else{
    x.out <- x

  }



  return(x.out)
}
