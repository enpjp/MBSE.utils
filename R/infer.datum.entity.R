#' Infer Datum Entity
#'
#' Checks to see if a system generated unique id exists. If it does, this is
#' used as a datumEntity, if not, one is randomly generated.
#'
#' @param x A syntactically correct XML file.
#' @param internal.id This is the XML attribute name used for unique ID. This
#'   default value is associated with Enterprise Architect.
#'
#' @return An XML file with datum Entity set.
#' @export infer.datum.entity
#'
infer.datum.entity <- function(x, internal.id = 'xmi.id') {

  datum.entity.value <- "default"

  has.xmi.id <-   xml2::xml_has_attr(x, internal.id)



  if(TRUE ){
      # Generate hash if there is no xmi.id

      if( has.xmi.id){

        datum.entity.value <- xml2::xml_attr(x, internal.id)
      }else{
        generate.hash <- rlang::hash(Sys.time())
        datum.entity.value <- paste("MBSE_",generate.hash, sep = "")

        #x.parent <-  xml2::xml_parent(x)
        #datum.entity.value <- infer.datum.entity(x.parent, method)
      }



  }



  return(datum.entity.value)
}
