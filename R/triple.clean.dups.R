#' Triple Clean Duplicates
#'
#' An internal function used to clean duplicate IDs.
#'
#' @param x Set of triples
#'
#' @return Set of triples with duplicates with new datumEntity values
#' @export triple.clean.dups
#'
#'
triple.clean.dups <- function(x) {

  triples.out.01 <- x

  model.triples.wide.01 <- x %>% tidyr::pivot_wider(
    id_cols = "datumEntity",
    names_from = "datumAttribute",
    values_fn = list,
    values_from = "datumValue"
  ) %>%  dplyr::mutate_all(as.character)

  problem.values.01 <- triples.out.01 %>%  dplyr::summarise(Duplicates = dplyr::n(), .by = c(.data$datumEntity, .data$datumAttribute)) %>%
    dplyr::filter(.data$Duplicates > 1L)

  problem.value.rows.01 <- triples.out.01[triples.out.01$datumEntity %in% problem.values.01$datumEntity,] %>%  dplyr::mutate_all(as.character)

  # Find the rows which need updating

  for(this.row in problem.value.rows.01$datumEntity){
    problem.row.numbers.01 <- which(triples.out.01$datumEntity == this.row)

    if(length(problem.row.numbers.01 > 0)){
      generate.hash <- rlang::hash(Sys.time())
      for( my.row in problem.row.numbers.01 ){

        triples.out.01$datumEntity[my.row] <- paste("DUPL_", generate.hash, sep = "")
        if(triples.out.01$datumAttribute[my.row] == "id"){
          triples.out.01$datumAttribute[my.row] <- "dup.id"
        }

        if(triples.out.01$datumAttribute[my.row] == "datumParent"){
          hash.attribute <- stringr::str_trunc( rlang::hash(Sys.time()), 5,  ellipsis = ""   )
          triples.out.01$datumAttribute[my.row] <- paste("stepParent_", hash.attribute, sep = "")

        }
      }


    }


  }


  # test.value.rows.01 <- triples.out.01[triples.out.01$datumValue %in% problem.values.01$datumEntity,]

  # Now we have found the problem rows and added them back in.

  return(triples.out.01)

}
