#' XML File To Triples
#'
#' @param the.file.path The path to the file directory
#' @param file.number A number - which file to use if there are more than 1.
#' @param xpath The Xpath. the default is root
#'
#' @return List of three values: triples; file.name; full.file.path.
#' @export xml.file.to.triples
#'
#'
xml.file.to.triples <- function( the.file.path = the.file.path,
                                 file.number = 1,
                                 xpath = "/*"){

  list.xml <- fs::dir_ls( path = the.file.path,
                          type="file", recurse = TRUE,
                          glob = "*.xml") # only these file types

  file_name_used <- list.xml[file.number] # Select file from list
  the_file_name <- fs::path_file(file_name_used) # Strip the path
  model_xml <- xml2::read_xml(file_name_used)   %>% xml2::xml_ns_strip()  #

  # Lazy print the filename
  #the_file_name ("//xmi:XMI)

  # Select the top level element - we know this from inspection of the file.
  model.elements.all <-  model_xml  %>% xml2::xml_find_all( xpath)



  triples.out.01  <- data.frame(
    datumEntity =character(),
    datumAttribute =character(),
    datumValue =character(),
    stringsAsFactors=FALSE)

  # Convert the file into triples
  for (i in seq_along(model.elements.all)) {

    model.triples.01 <- model.elements.all[i] %>%
      MBSE.utils::set.datum.entity() %>% # Set the datumEntity
      MBSE.utils::set.datum.parent() %>%  # Set the parent of the node
      MBSE.utils::make.datum.triple() %>%  # Turn in to triples
      MBSE.utils::get.tag.value.pairs() # Expand Tag Value Pairs

    triples.out.01 <- rbind(triples.out.01,  model.triples.01)
  }

  triples.out.01 <- triples.out.01 %>%  dplyr::mutate_all(as.character)

 # model.triples.wide.01 <-triples.out.01 %>% MBSE.utils::triple.as.wide()


  #cleaned.triples.out2 <- do.call(as.character, triples.out$datumEntity)
  cleaned.triples.out.01 <- triples.out.01 %>% triple.clean.dups()

  out.data <- list(
      "triples" = cleaned.triples.out.01,
      "file.name" = the_file_name,
      "file.full.path" = file_name_used

  )
  return(out.data)
}
