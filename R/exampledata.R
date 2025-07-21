#' Get example OSM data
#'
#' This function retrieves example OpenStreetMap (OSM) data from a persistent
#' URL on the 4TU.ResearchData data repository, and it can be used in examples
#' and tests. The code used to generate the example dataset is available at
#' https://github.com/CityRiverSpaces/CRiSpExampleData
#'
#' @srrstats {G5.0} The dataset used in examples, tests and vignettes is
#'   retrieved from a persistent location on a data repository. The dataset
#'   follows FAIR data standards and thus its properties are described in
#'   detail.
#' @srrstats {G5.1} The example dataset is made generally available to users
#'   through the `get_osm_example_data()` function which retrieves the data
#'   from a persistent URL. The code used to generate the example dataset is
#'   publicly available, as described in the Roxygen comments.
#'
#' @return A list of sf objects containing the OSM data as [`sf::sfc`]
#'   objects.
#' @importFrom utils download.file
#' @importFrom stats setNames
#' @export
#'
#' @examplesIf interactive()
#' get_osm_example_data()
#' @srrstats {SP4.0, SP4.0b, SP4.2} The function returns a list of
#'   [`sf::sfc`] objects, explicitly documented as such.
get_osm_example_data <- function() {
  url_osm <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/f519315e-b92d-4815-b924-3175bd2a7a61"  # nolint
  temp_file <- tempfile(fileext = ".gpkg")
  # temporarily increate timeout, reset value on exit
  op <- options(timeout = 300)
  on.exit(options(op))
  download.file(url_osm, destfile = temp_file, mode = "wb", quiet = TRUE)
  names <- sf::st_layers(temp_file)$name
  lapply(names, \(layer) sf::st_read(temp_file, layer = layer, quiet = TRUE)) |>
    setNames(names)
}

#' Get example DEM data
#'
#' This function retrieves example Digital Elevation Model (DEM) data from a
#' persistent URL on the 4TU.ResearchData data repository, and it can be used
#' in examples and tests. The code used to generate the example dataset is
#' available at https://github.com/CityRiverSpaces/CRiSpExampleData.
#'
#' @return A [`terra::SpatRaster`] object containing the DEM data.
#' @export
#'
#' @examplesIf interactive()
#' get_dem_example_data()
#' @srrstats {SP4.0, SP4.0b, SP4.2} The function returns a [`terra::SpatRaster`]
#'   object, explicitly documented as such.
get_dem_example_data <- function() {
  url_dem <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/9eeee7aa-6005-48d6-ad63-d11c479db88b"  # nolint
  terra::rast(url_dem)
}
