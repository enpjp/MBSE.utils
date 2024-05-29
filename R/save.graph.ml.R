#' Save Graph In ML Format
#'
#' Saves a graph object as Ml so it can be read by Gephi.
#'
#' @param x Graph object
#' @param name.prefix Name prefix for the file
#'
#' @return x unchanged and saves file.
#' @export save.graph.ml
#'
#'
save.graph.ml <- function(x, name.prefix = "subgraph") {

  path.to.gephi <-  fs::path("data-gephi")
  fs::dir_create(path.to.gephi)
  # Added to drop duplicates
# x.clean <- igraph::V(x) %>% unique()
# x.cleaner <- igraph::delete_vertices(x,x.clean)

  path.to.file <- fs::path(path.to.gephi, name.prefix, ext = "gml")

igraph::write_graph(x,path.to.file, format = "gml")

  return(x)
}
