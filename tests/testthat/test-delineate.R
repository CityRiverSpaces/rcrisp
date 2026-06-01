test_osmdata <- get_test_osmdata()
test_dem <- get_test_dem_valley(
  test_osmdata$river_centerline, ymin = -10000, ymax = 10000, res = 50
)
crs <- sf::st_crs(test_osmdata$river_centerline)

#' @srrstats {G5.8} Edge test: an error is raised if conflicting input
#'   parameters are given.
test_that("Segmentation without corridor raises error", {
  expect_error(delineate("Bucharest", "Dâmbovița",
                         corridor = FALSE, segment = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Delineate returns all required delineation units", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  with_mocked_bindings(get_osmdata = function(...) test_osmdata,
                       get_dem = function(...) test_dem,
                       delineations <- delineate(city_name = "MyCity",
                                                 river_name = "MyRiver",
                                                 crs = crs,
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
                  c("streets", "railways", "river_centerline", "river_surface",
                    "valley", "corridor", "segments", "riverspace"))
  geometry_types <- sapply(delineations, sf::st_geometry_type)
  # segments include multiple geometries, flatten array for comparison
  expect_in(do.call(c, geometry_types),
            c("POLYGON", "MULTIPOLYGON", "LINESTRING", "MULTILINESTRING"))
})

test_that("Delineate does not return the valley if the buffer method is used", {
  # Input arguments should mimic as much as possible the input used to setup
  # the example datasets, see:
  # https://github.com/CityRiverSpaces/CRiSpExampleData/blob/main/data-raw/bucharest.R  # nolint
  with_mocked_bindings(get_osmdata = function(...) test_osmdata,
                       get_dem = function(...) test_dem,
                       delineations <- delineate(city_name = "MyCity",
                                                 river_name = "MyRiver",
                                                 crs = crs,
                                                 network_buffer = 2500,
                                                 buildings_buffer = 100,
                                                 dem_buffer = 2500,
                                                 corridor_init = 1000,
                                                 corridor = TRUE,
                                                 # only compute corridor here
                                                 segments = FALSE,
                                                 riverspace = FALSE) |>
                         suppressWarnings())
  expect_setequal(names(delineations),
                  c("streets", "railways", "river_centerline", "river_surface",
                    "corridor"))
})

test_that("If `network_buffer` is not specified, the default value is used", {
  expect_message(with_mocked_bindings(get_osmdata = \(...) test_osmdata,
                                      get_dem = \(...) test_dem,
                                      delineate(city_name = "MyCity",
                                                river_name = "MyRiver",
                                                crs = crs) |>
                                        suppressWarnings()),
                 paste0("The default `network_buffer` of 3000 m ",
                        "is used for corridor delineation."))
})

test_that("If `buildings_buffer` is not specified, the default value is used", {
  expect_message(with_mocked_bindings(get_osmdata = \(...) test_osmdata,
                                      get_dem = \(...) test_dem,
                                      delineate(city_name = "MyCity",
                                                river_name = "MyRiver",
                                                crs = crs,
                                                riverspace = TRUE) |>
                                        suppressWarnings()),
                 paste0("The default `buildings_buffer` of 100 m ",
                        "is used for riverspace delineation."))
})

#' @srrstats {G5.8} Edge test: an error is raised if the dimension of the input
#'   parameters does not fit the requirements.
test_that("Only one city can be delineated at a time", {
  expect_error(delineate(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
})
