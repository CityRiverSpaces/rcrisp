#' Get example OSM data
#'
#' This function retrieves example OpenStreetMap (OSM) data from a persistent
#' URL on the 4TU.ResearchData data repository. The data is in the form of a
#' list of sf objects, which can be used in examples and tests.
#'
#' @return A list of sf objects containing the OSM data.
#' @export
#'
#' @examplesIf interactive()
#' get_osm_example_data()
get_osm_example_data <- function() {
  # nolint start
  url_osm <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/f519315e-b92d-4815-b924-3175bd2a7a61"
  # nolint end
  sf::st_read(url_osm)
}

#' Get example DEM data
#'
#' This function retrieves example Digital Elevation Model (DEM) data from a
#' persistent URL on the 4TU.ResearchData data repository. The data is in the
#' form of a SpatRaster object, which can be used in examples and tests.
#'
#' @return A SpatRaster object containing the DEM data.
#' @export
#'
#' @examplesIf interactive()
#' get_dem_example_data()
get_dem_example_data <- function() {
  # nolint start
  url_dem <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/9eeee7aa-6005-48d6-ad63-d11c479db88b"
  # nolint end
  terra::rast(url_dem)
}
