test_osm <- get_test_osm()
test_dem <- get_test_dem_valley(
  test_osm$river_centerline, ymin = -10000, ymax = 10000, res = 50
)
crs <- sf::st_crs(test_osm$river_centerline)

aoi <- list(
  city_name = "Bucharest",
  river_name = "Dâmbovița",
  bb = sf::st_bbox(c(xmin = 25.967,
                     ymin = 44.334,
                     xmax = 26.226,
                     ymax = 44.541),
                   crs = "EPSG:4326"),
  crs = crs,
  network_buffer = 3000,
  dem_buffer = 2500,
  buildings_buffer = 100
)

#' @srrstats {G5.8} Edge test: an error is raised if conflicting input
#'   parameters are given.
test_that("Segmentation without corridor raises error", {
  expect_error(delineate(aoi,
                         test_osm,
                         corridor = FALSE,
                         segments = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Delineate returns all required delineation units", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  delineations <- delineate(aoi,
                            test_osm,
                            test_dem,
                            corridor = TRUE,
                            segments = TRUE,
                            riverspace = TRUE) |>
    suppressMessages() |>
    suppressWarnings()
  expect_setequal(names(delineations),
                  c("valley", "corridor", "segments", "riverspace"))
  expect_true(all(vapply(
    delineations,
    \(x) inherits(x, c("sfc_POLYGON", "sfc_MULTIPOLYGON")),
    logical(1)
  )))
})

test_that("Delineate does not return the valley if the buffer method is used", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  delineations <- delineate(aoi,
                            test_osm,
                            corridor_init = 1000,
                            corridor = TRUE,
                            segments = FALSE,
                            riverspace = FALSE) |>
    suppressWarnings()
  expect_equal(names(delineations), "corridor")
})

#' @srrstats {G5.8} Edge test: an error is raised if the dimension of the input
#'   parameters does not fit the requirements.
test_that("Only one city can be delineated at a time", {
  expect_error(delineate_city_river(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
})

#' @srrstats {G5.8} Edge test: an error is raised if required OSM data is
#'   missing.
test_that("Error is raised when OSM spatial network data is missing", {
  osm_without_streets <- test_osm
  osm_without_streets$streets <- NULL
  expect_error(delineate(aoi, osm_without_streets, test_dem, corridor = TRUE),
               "Spatial network \\(streets, railways\\) data is not available")
})

test_that("Error is raised when buildings data is missing for riverspace delineation", {  # nolint
  osm_without_buildings <- test_osm
  osm_without_buildings$aoi_buildings <- NULL
  expect_error(
    delineate(aoi, osm_without_buildings, test_dem, riverspace = TRUE),
    "AOI for buildings is not available"
  )
})
