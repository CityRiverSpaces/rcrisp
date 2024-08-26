#' Retrieve bounding box from OpenStreetMap
#'
#' @param name A character string with the name of the place to retrieve
#'             the bounding box
#'
#' @return A list with the bounding box
#' @export
#'
#' @examples
#' osm_bb("Bucharest")
osm_bb <- function(name) {
  osmdata::getbb(name)
}

#' Retrieve OpenStreetMap data as sf object
#'
#' Query the Overpass API for a key:value pair within a given bounding box.
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
#' @param name A character string with the name of the place to retrieve
#'             the bounding box
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#'
#' @return An sf object with the retrieved OpenStreetMap data for the
#'         given location
#' @export
#'
#' @examples
#' get_osmdata("Bucharest", "waterway", "river")
get_osmdata <- function(name, key, value) {
  bb <- CRiSp::osm_bb(name)
  CRiSp::osmdata_as_sf(key, value, bb)
}

#' Get the city boundary from OpenStreetMap
#'
#' @param city_name A character string with the name of the city
#'
#' @return An sf object with the city boundary
#' @export
#'
#' @examples
#' get_osm_city_boundary("Bucharest")
get_osm_city_boundary <- function(city_name) {
  city_boundary <- CRiSp::get_osmdata(city_name, "boundary", "administrative")
  if (is.null(city_boundary)) {
    city_boundary <- CRiSp::get_osmdata(city_name, "place", "city")
  } else {
    stop("No city boundary found")
  }
}

#' Get OpenStreetMap data for a river corridor
#'
#' @param city_name A character string with the name of the place to retrieve
#'                  the bounding box
#' @param river_name A character string with the name of the river
#' @param epsg_code An integer with the EPSG code for the projection
#' @param buffer_dist A numeric with the buffer distance in meters from the
#'                    water stream
#'
#' @return An sf object with the river corridor
#' @export
get_osmdata_river_corridor <- function(city_name,
                                       river_name,
                                       epsg_code,
                                       buffer_dist) {
  key <- "waterway"
  value <- "river"
  waterways <- CRiSp::get_osmdata(city_name, key, value)
  waterway <- waterways$osm_multilines |>
    dplyr::filter(name == river_name) |>
    sf::st_transform(epsg_code) |>
    sf::st_geometry()

  key <- "natural"
  value <- "water"
  water <- CRiSp::get_osmdata(city_name, key, value)

  waterbody <- dplyr::bind_rows(water$osm_polygons, water$osm_multipolygons) |>
    sf::st_transform(epsg_code) |>
    sf::st_filter(waterway, .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  corridor_initial <- c(waterway, waterbody) |>
    sf::st_buffer(buffer_dist) |>
    sf::st_union()

  corridor_initial
}
