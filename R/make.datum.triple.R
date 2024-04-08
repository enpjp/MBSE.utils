#' Make Datum Triple
#'
#' Extracts attributes from a XML element and presents them as a datum Triple in
#' the form of a three column data frame.
#'
#' @param x A syntactically correct XML file with datumEntity and other
#'   attributes set.
#'
#' @return A data.frame of triples datumEntity, datumAttribute, datumValue.
#' @export make.datum.triple
#'
#'
make.datum.triple <- function(x) {
  x.out  <- data.frame(
                            datumEntity =character(),
                            datumAttribute =character(),
                            datumValue =character(),
                            stringsAsFactors=FALSE)

  # Find the base attributes as triple

  triples.out <- x %>%
    xml.attrs.to.dataframe() %>% data.as.triple()
  x.out <- rbind(x.out, triples.out)

  # Get the children of the base element
  children.elements <- x  %>% xml2::xml_children()
    if(length(children.elements) > 0 ) {
        for(i in seq_along(children.elements)  ) {
          triples.out <- children.elements[i] %>%
            xml.attrs.to.dataframe() %>% data.as.triple()

          x.out <- rbind(x.out, triples.out)


          # And the recursive bit
          children.of.children <- children.elements[i]  %>% xml2::xml_children()

          if(length(children.of.children) > 0) {
            for(j in seq_along(children.of.children)   ) {
              recurse.out <-   make.datum.triple(children.of.children[j])
              # recurse out always includes at least children.elements[i]
              x.out <- rbind(x.out, recurse.out)

            }



          }

        }

    }


  # x.attrs <- xml2::xml_attrs(x) %>% purrr::list_flatten()
  #
  # # Convert to data frame
  # raw.df <-  data.frame(as.list(x.attrs)) %>% t()
  # row.names(raw.df)  <- NULL
  # df.tibble <- as_tibble(raw.df)
  # triple.out <- data.as.triple(df.tibble)
  # x.out <- rbind(x.out, triple.out)
  #
  # # Siblings
  #
  # x.siblings <- xml2::xml_siblings(x)
  # if( length(x.siblings) > 10000) {
  #   # Yes we have siblings
  #   for(i in seq_along(x.siblings)) {
  #     sibling <- x.siblings[i]
  #     #get the attributes
  #     sibling.attrs <- xml2::xml_attrs(sibling) %>% purrr::list_flatten()
  #     # Convert to data frame
  #     raw.df <-  data.frame(as.list(sibling.attrs)) %>% t()
  #     row.names(raw.df)  <- NULL
  #     df.tibble <- as_tibble(raw.df)
  #     triple.out <- data.as.triple(df.tibble)
  #     x.out <- rbind(x.out, triple.out)
  #
  #
  #   }
  #
  # }

  # We know that we will always have duplicated elements so drop them.
  x.out <- x.out[!(duplicated(x.out) | duplicated(x.out, fromLast = TRUE)), ]

  return(x.out)
}
