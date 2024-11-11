as_network <- function(edges, crs = NULL, flatten = FALSE, clean = FALSE) {
  network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
  if (!is.null(crs)) network <- sf::st_transform(network, crs)
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