#' Triple As Wide
#'
#' Expands datumTriples into a data.frame with one row per datumEntity and on
#' column per datumAttribue datumValue pair.
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
  )

  return(data.out)
}
