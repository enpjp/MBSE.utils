#' Triple As Wide
#'
#' A convenience function that expands datumTriples into a data.frame with one
#' row per datumEntity and one column per attribute datumValue pair.
#'
#' @param x Three column datum Triple format data.
#'
#' @return Wide data.frame.
#' @export triple.as.wide
#'
#'
triple.as.wide <- function(x) {
  data.out <- tidyr::pivot_wider(x,
    id_cols = "datumEntity",
    names_from = "datumAttribute",
    values_fn = list,
    values_from = "datumValue"
  ) %>%  dplyr::mutate_all(as.character)

  return(data.out)
}
