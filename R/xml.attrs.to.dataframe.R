#' XML Attributes To Data Frame
#'
#' A convenience function to extract XML attributes from a node and return them
#' as a data.frame.
#'
#' @param x An XML node.
#'
#' @return A data frame containing the XML attributes.
#'
#'
#'
xml.attrs.to.dataframe <- function(x) {
  # Get the attributes
  x.attrs <- xml2::xml_attrs(x)
  # Convert to data frame
  raw.df <- data.frame(as.list(x.attrs))
  # Transform rows and columns and drop row names
  df.out <- t(raw.df)
  row.names(df.out) <- NULL

  # Convert to a tibble to improve Tidyverse compatibility.
  df.tibble <- tidyr::as_tibble(df.out)




  return(df.tibble)
}
