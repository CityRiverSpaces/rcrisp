#' Retrieve bounding box from OpenStreetMap
#'
#' @param name A character string with the name of the place to retrieve the bounding box
#'
#' @return A list with the bounding box
#' @export
#'
#' @examples
#' osm_bb("Bucharest")
osm_bb <- function(name) {
  bb <- osmdata::getbb(name)
}

#' Retrieve OpenStreetMap data as sf object
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param bb A list with the bounding box
#'
#' @return An sf object with the retrieved OpenStreetMap data
#' @export
osmdata_as_sf <- function(key, value, bb) {
  bb |>
    osmdata::opq() |>
    osmdata::add_osm_feature(key = key, value = value) |>
    osmdata::osmdata_sf()
}

#' Retrieve OpenStreetMap data for a given location
#'
#' @param name A character string with the name of the place to retrieve the bounding box
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#'
#' @return An sf object with the retrieved OpenStreetMap data for the given location
#' @export
#'
#' @examples
#' get_osmdata("Bucharest", "waterway", "river")
get_osmdata <- function(name, key, value) {
  bb <- CRiSp::osm_bb(name)
  CRiSp::osmdata_as_sf(key, value, bb)
}

#' Get OpenStreetMap data for a river corridor
#'
#' @param name A character string with the name of the place to retrieve the bounding box
#' @param river_name A character string with the name of the river
#' @param epsg_code An integer with the EPSG code for the projection
#' @param buffer A numeric with the buffer distance in meters from the water stream
#'
#' @return An sf object with the river corridor
#' @export
get_osmdata_river_corridor <- function(name, river_name, epsg_code, buffer) {
  key = "waterway"
  value = "river"
  waterways <- CRiSp::get_osmdata(name, key, value)
  waterway <- waterways$osm_multilines |>
    dplyr::filter(name == river_name) |>
    sf::st_transform(epsg_code) |>
    sf::st_geometry()

  key = "natural"
  value = "water"
  water <- CRiSp::get_osmdata(name, key, value)

  waterbody <- dplyr::bind_rows(water$osm_polygons, water$osm_multipolygons) |>
    sf::st_transform(epsg_code) |>
    sf::st_filter(waterway, .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  corridor_initial <- c(waterway, waterbody) |>
    sf::st_buffer(buffer) |>
    sf::st_union()
}
