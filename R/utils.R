#' Add latitude and longitude information to an sf object
#'
#' Add latitude and longitude information to an sf object (WGS84)
#'
#' @param x Object of class sf, sfc or sfg
#'
#' @return An sf object with latitude and longitude
#' @export
get_geom_latlon <- function(x) {
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

#' Get UTM zone from longitude
#'
#' @param x An sf object
#'
#' @return The UTM zone
#' @export
get_utm_zone_epsg <- function(x) {
  if (!"sf" %in% class(x)) {
    stop("x must be an sf object")
  }

  coords <- x |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_centroid() |>
    sf::st_transform(4326) |>
    sf::st_coordinates()

  if (coords[2] >= 0L) {
    base <- 32600
  } else {
    base <- 32700
  }
  base + floor((coords[1] + 180) / 6) + 1
}
