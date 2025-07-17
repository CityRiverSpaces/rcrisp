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
                         suppressMessages() |>
                         suppressWarnings())
  expect_setequal(names(delineations),
                  c("valley", "corridor", "segments", "riverspace"))
  expect_true(all(sapply(
    delineations, \(x) inherits(x, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))
  )))
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

test_that("Only one city and one river can be delineated at a time", {
  expect_error(delineate(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
  expect_error(delineate("Bucharest", c("Dâmbovița", "SomeOtherRiver")),
               "Assertion on 'river_name' failed: Must have length 1")
})
