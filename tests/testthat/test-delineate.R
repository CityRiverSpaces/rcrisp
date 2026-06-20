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
                  c("streets", "railways", "river_centerline", "river_surface",
                    "valley", "corridor", "segments", "riverspace", "aoi"))
  spatial_layers <- Filter(\(x) inherits(x, c("sf", "sfc")), delineations)
  geometry_types <- sapply(spatial_layers, sf::st_geometry_type)
  # segments include multiple geometries, flatten array for comparison
  expect_in(do.call(c, geometry_types),
            c("POLYGON", "MULTIPOLYGON", "LINESTRING", "MULTILINESTRING"))
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
  expect_equal(names(delineations),
               c("streets", "railways", "river_centerline", "river_surface",
                 "corridor", "aoi"))
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

# Minimal delineation object for print/summary snapshot tests.
# Using a projected CRS (UTM zone 35N) and simple geometries with known
# dimensions: a 1x1 km square and a 2 km line.
square <- sf::st_sfc(
  sf::st_polygon(list(
    rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0))
  )),
  crs = 32635
)
line <- sf::st_sfc(
  sf::st_linestring(rbind(c(0, 500), c(2000, 500))),
  crs = 32635
)
minimal_bd <- structure(
  list(
    streets = sf::st_sf(type = "primary", geometry = square),
    railways = NULL,
    river_centerline = line,
    river_surface = square,
    valley = square,
    corridor = square,
    segments = c(square, square),
    riverspace = NULL,
    aoi = list(
      city_name = "TestCity",
      river_name = "TestRiver",
      network_buffer = 3000,
      dem_buffer = 2500,
      buildings_buffer = 100
    )
  ),
  class = c("delineation", "list")
)

test_that("print.delineation output matches snapshot", {
  expect_snapshot(print(minimal_bd))
})

test_that("summary.delineation and print.summary.delineation match snapshot", {
  expect_snapshot(summary(minimal_bd))
})
