#' Add latitude and longitude information to an sf object
#'
#' Add latitude and longitude information to an sf object (WGS84)
#'
#' @param x Object of class sf, sfc or sfg
#'
#' @return An sf object with latitude and longitude
#' @export
get_latlon <- function(x) {
  sf::st_transform(x, 4326) |>
    sf::st_geometry()
}

#' Check if two sf objects do not intersect
#'
#' @param x sf object
#' @param y sf object
#'
#' @return A logical vector
#' @export
not_intersects <- function(x, y) {
  !sf::st_intersects(x, y)
}

#' Calculate the weights of the edges of a network based on their length
#'
#' @param net A network object
#'
#' @return A network object with a new column 'weight' containing the length of
#'         the edges
#' @export
calc_weights <- function(net) {
  net |>
    sfnetworks::activate("edges") |>
    dplyr::mutate(weight = sfnetworks::edge_length())
}
