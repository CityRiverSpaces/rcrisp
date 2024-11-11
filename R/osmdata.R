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
osmdata_as_sf <- function(key, value, bb) {
  bb |>
    osmdata::opq() |>
    osmdata::add_osm_feature(key = key, value = value) |>
    osmdata::osmdata_sf()
}

#' Retrieve OpenStreetMap data for a given location
#'
#' Retrieve OpenStreetMap data for a given location, including
#' the city boundary, the river centreline and surface, the streets, and the
#' railways.
#'
#' @param city_name A character string with the name of the city.
#' @param river_name A character string with the name of the river.
#' @param crs An integer with the EPSG code for the projection. If no CRS is
#'            specified, the default is the UTM zone for the city.
#' @param buffer A numeric with the buffer distance in meters. By default,
#'               no buffer is applied
#'
#' @return An list with the retrieved OpenStreetMap data sets for the
#'         given location
#' @export
#'
#' @examples
#' get_osmdata("Bucharest", "Dambovita", buffer = 2000)
get_osmdata <- function(city_name, river_name, crs = NULL, buffer = NULL) {
  bb <- osmdata::getbb(city_name)
  if (is.null(crs)) crs <- get_utm_zone_epsg_bb(bb)

  bb <- bb |> as.vector()
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bb <- st_bbox(bb, crs = 4326)

  if (!is.null(buffer)) {
    bb <- bb |>
      st_as_sfc() |>
      st_transform(crs = crs) |>
      st_buffer(buffer) |>
      st_transform(crs = 4326) |>
      st_bbox()
  }

  boundary <- get_osm_city_boundary(city_name, bb, crs)
  river <- get_osm_river(river_name, bb, crs)
  srteets <- get_osm_streets(bb, crs)
  railways <- get_osm_railways(bb, crs)

  osm_data <- list(
    bb = bb,
    boundary = boundary,
    river_centerline = river$centerline,
    river_surface = river$surface,
    streets = srteets,
    railways = railways
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
#' get_osm_city_boundary("Bucharest", bb, crs)
get_osm_city_boundary <- function(city_name, bb, crs) {
  # Define a helper function to fetch the city boundary
  fetch_boundary <- function(key, value, ...) {
    CRiSp::osmdata_as_sf(key, value, bb)$osm_multipolygons |>
      dplyr::filter(dplyr::if_any(c(.data$`name:en`, .data$name),
                                  ~ . == stringr::str_extract(city_name,
                                                              "^[^,]+"))) |>
      sf::st_transform(crs) |>
      sf::st_geometry()
  }

  # Try to get the city boundary with the "place:city" tag
  city_boundary <- tryCatch(fetch_boundary("place", "city"),
                            error = function(e) NULL)

  # If not found, try again with the "boundary:administrative" tag
  if (is.null(city_boundary)) {
    city_boundary <- tryCatch(fetch_boundary("boundary", "administrative"),
                              error = function(e) NULL)
  }

  # If still not found, throw an error
  if (is.null(city_boundary)) stop("No city boundary found")

  city_boundary
}

#' Get the river centreline and surface from OpenStreetMap
#'
#' @param river_name The name of the river
#' @param bb Bounding box of class `bbox`
#' @param crs Coordinate reference system as EPSG code
#'
#' @return A list with the river centreline and surface
#' @export
#'
#' @examples
#' get_osm_river("Dâmbovița", bb, crs)
get_osm_river <- function(river_name, bb, crs) {
  # Get the river centreline
  river_centerline <- CRiSp::osmdata_as_sf("waterway", "river", bb)
  river_centerline <- river_centerline$osm_multilines |>
    dplyr::filter(.data$name == river_name) |>
    sf::st_transform(crs) |>
    sf::st_geometry()

  # Get the river surface
  river_surface <- CRiSp::osmdata_as_sf("natural", "water", bb)
  river_surface <- dplyr::bind_rows(river_surface$osm_polygons,
                                    river_surface$osm_multipolygons) |>
    sf::st_transform(crs) |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  return(list(centerline = river_centerline,
              surface = river_surface))
}

#' Get OpenStreetMap streets
#'
#' @param bb Boundary box
#' @param crs Coordinate reference system as EPSG code
#' @param highway_values A character vector with the highway values to retrieve.
#'             If left NULL, the function retrieves the following values:
#'             "motorway", "trunk", "primary", "secondary", "tertiary"
#' @return An sf object with the streets
#' @export
#'
#' @examples
#' get_osm_streets(bb, crs)
get_osm_streets <- function(bb, crs, highway_values = NULL) {
  if (is.null(highway_values)) {
    highway_values <- c("motorway", "trunk", "primary", "secondary", "tertiary")
  }

  link_values <- sapply(X = highway_values,
                        FUN = \(x) sprintf("%s_link", x),
                        USE.NAMES = FALSE)

  streets <- osmdata_as_sf("highway", c(highway_values, link_values), bb)

  # Cast polygons (closed streets) into lines
  poly_to_lines <- streets$osm_polygons |>
    sf::st_cast("LINESTRING")

  # Combine all features in one data frame
  streets_lines <- streets$osm_lines |>
    dplyr::bind_rows(poly_to_lines) |>
    dplyr::select("highway") |>
    dplyr::rename(type = `highway`) |>
    sf::st_transform(crs)

  return(streets_lines)
}

#' Get OpenStreetMap railways
#'
#' @param bb Bounding box
#' @param crs Coordinate reference system as EPSG code
#'
#' @return An sf object with the railways
#' @export
#'
#' @examples
#' get_osm_railways(bb, crs)
get_osm_railways <- function(bb, crs) {
  railways <- osmdata_as_sf("railway", "rail", bb)
  railways_lines <- railways$osm_lines |>
    dplyr::select("railway") |>
    dplyr::rename(type = `railway`) |>
    sf::st_transform(crs)

  return(railways_lines)
}
