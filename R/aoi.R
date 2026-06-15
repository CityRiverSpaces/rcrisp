#' Define delineation parameters within an area of interest
#'
#' @param city_name A character vector of length one
#' @param river_name A character vector of length one
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param network_buffer Add a buffer (an integer in meters) around
#'   river to retrieve additional data (streets, railways, etc.).
#'   Default is 3000 m.
#' @param dem_buffer Size of the buffer region (in meters) around the
#'   river to retrieve the DEM  (only used if `corridor_init` is `"valley"` and
#'   `dem` is NULL).
#' @param buildings_buffer Add a buffer (an integer in meters) around the
#'   river to retrieve additional data (buildings). Default is 100 m.
#'
#' @return A list with delineation parameter values, namely `city_name` and
#'   `river_name` as character vectors of length one, `bb` as object of class
#'   [`sf::bbox`][sf::st_bbox], `crs` as object of class [`sf::crs`],
#'   `corridor_init` as character or numerical vector of length one, and
#'   `network_buffer`, `dem_buffer` and `buildings_buffer` as numerical vector
#'   of length one.
#' @export
#' @examplesIf interactive()
#' # Get default parameters in AOI
#' aoi <- define_aoi("Bucharest", "Dâmbovița")
#'
#' # Get parameters in AOI with custom CRS
#' aoi <- define_aoi("Bucharest", "Dâmbovița",
#'                   crs = "EPSG:3844")  # National projected CRS
#'
#' # Get parameters in AOI with non-default initial corridor buffer
#' aoi <- define_aoi("Bucharest", "Dâmbovița", corridor_init = 1000)
#'
#' # Get parameters in AOI with non-default buffers
#' aoi <- define_aoi("Bucharest", "Dâmbovița",
#'                   network_buffer = 2000,
#'                   dem_buffer = 2000,
#'                   buildings_buffer = 150)
define_aoi <- function(
  city_name, river_name,
  crs = NULL,
  network_buffer = 3000, dem_buffer = 5500, buildings_buffer = 100
) {
  # Check input
  checkmate::assert_character(city_name, len = 1)
  checkmate::assert_character(river_name, len = 1)
  checkmate::assert_numeric(network_buffer, len = 1)
  checkmate::assert_numeric(dem_buffer, len = 1)
  checkmate::assert_numeric(buildings_buffer, len = 1)

  bb <- get_osm_bb(city_name)

  # If not provided, determine the CRS. Otherwise, standardise CRS
  if (is.null(crs)) {
    crs <- get_utm_zone(bb) |> as_crs()
  } else {
    crs <- as_crs(crs)
  }

  list(
    city_name = city_name,
    river_name = river_name,
    bb = bb,
    crs = crs,
    network_buffer = network_buffer,
    dem_buffer = dem_buffer,
    buildings_buffer = buildings_buffer
  )
}
