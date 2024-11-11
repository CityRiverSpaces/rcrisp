#' Retrieve OpenStreetMap data as sf object
#'
#' Query the Overpass API for a key:value pair within a given bounding box.
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param bb A matrix with the bounding box (rows for "x", "y", columns for
#'           "min", "max")
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
#' @param
#'
#' @return An list with the retrieved OpenStreetMap data sets for the
#'         given location
#' @export
#'
#' @examples
#' get_osmdata()
get_osm_data <- function(city_name, river_name, crs = NULL, buffer = NULL) {
  bb <- osmdata::getbb(city_name)

  if (!is.null(buffer)) bb <- ...
  if (!is.null(crs)) crs <- ...

  boundary <- get_osm_city_boundary(bb, crs)
  river <- get_osm_river(bb, crs)
  highways <- get_osm_highways(bb, crs)
  railways <- get_osm_railways(bb, crs)

  osm_data <- list(
    bb = bb,
    boundary = boundary,
    river_centerline = river[["centerline"]],
    river_surface = river[["surface"]],
    highways = highways,
    railways = railways,
  )
  return(osm_data)
}

#' Get the city boundary from OpenStreetMap
#'
#' @param city_name A character string with the name of the city
#'
#' @return An sf object with the city boundary
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' get_osm_city_boundary("Bucharest")
get_osm_city_boundary <- function(city_name) {
  fetch_boundary <- function(key, value) {
    CRiSp::get_osmdata(city_name, key, value)$osm_multipolygons |>
      dplyr::filter(dplyr::if_any(c(.data$`name:en`, .data$name),
                                  ~ . == stringr::str_extract(city_name,
                                                              "^[^,]+"))) |>
      sf::st_geometry()
  }

  city_boundary <- tryCatch(fetch_boundary("place", "city"),
                            error = function(e) NULL)

  if (is.null(city_boundary)) {
    city_boundary <- tryCatch(fetch_boundary("boundary", "administrative"),
                              error = function(e) NULL)
  }

  if (is.null(city_boundary)) stop("No city boundary found")

  city_boundary
}

get_osm_river <- function(crs = NULL) {

}

get_osm_highways <- function() {

}

get_osm_railways <- function() {

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
#' @importFrom rlang .data
#' @export
get_osmdata_river_corridor <- function(city_name,
                                       river_name,
                                       epsg_code,
                                       buffer_dist) {
  key <- "waterway"
  value <- "river"
  waterways <- CRiSp::get_osmdata(city_name, key, value)
  waterway <- waterways$osm_multilines |>
    dplyr::filter(.data$name == river_name) |>
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
