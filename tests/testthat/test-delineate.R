test_that("Segmentation without corridor raises error", {
  expect_error(delineate("Bucharest", "Dâmbovița",
                         corridor = FALSE, segment = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Delineate returns all required delineation units", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  with_mocked_bindings(get_osmdata = function(...) bucharest_osm,
                       get_dem = function(...) bucharest_dem,
                       delineations <- delineate(city_name = "Bucharest",
                                                 river_name = "Dâmbovița",
                                                 crs = 32635,
                                                 network_buffer = 2500,
                                                 buildings_buffer = 100,
                                                 dem_buffer = 2500,
                                                 corridor_init = "valley",
                                                 corridor = TRUE,
                                                 segments = TRUE,
                                                 riverspace = TRUE) |>
                         suppressWarnings())
  expect_setequal(names(delineations),
                  c("valley", "corridor", "segments", "riverspace"))
  geometry_types <- sapply(delineations, sf::st_geometry_type)
  # segments include multiple geometries, flatten array for comparison
  expect_in(do.call(c, geometry_types), c("POLYGON", "MULTIPOLYGON"))
})

test_that("Delineate does not return the valley if the buffer method is used", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  with_mocked_bindings(get_osmdata = function(...) bucharest_osm,
                       get_dem = function(...) bucharest_dem,
                       delineations <- delineate(city_name = "Bucharest",
                                                 river_name = "Dâmbovița",
                                                 crs = 32635,
                                                 network_buffer = 2500,
                                                 buildings_buffer = 100,
                                                 dem_buffer = 2500,
                                                 corridor_init = 1000,
                                                 corridor = TRUE,
                                                 # only compute corridor here
                                                 segments = FALSE,
                                                 riverspace = FALSE) |>
                         suppressWarnings())
  expect_equal(names(delineations), "corridor")
})
