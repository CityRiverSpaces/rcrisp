#' Create a network from a line strings
#'
#' @param edges A data frame with the network edges
#' @param flatten Whether all intersections between edges should be
#'                converted to nodes
#' @param clean Whether to run general cleaning tasks on the generated network
#'
#' @return A network object
#' @export
as_network <- function(edges, flatten = FALSE, clean = FALSE) {
  network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
  if (flatten) network <- flatten_network(network)
  if (clean) network <- clean_network(network)
  return(network)
}

flatten_network <- function(network) {

  return(network)
}

clean_network <- function(network) {

  return(network)
}

add_weights <- function() {

}

shortest_path <- function() {

}

strokes <- function() {

}