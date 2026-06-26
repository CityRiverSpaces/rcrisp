#' @srrstats {G5.8} Edge test: an error is raised if the dimension of the input
#'   parameters does not fit the requirements.
test_that("Parameters can be set for only one city at a time", {
  expect_error(define_aoi(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
})

#' @srrstats {G5.8} Edge test: NULL values are rejected for city and river names
test_that("NULL values are rejected for required parameters", {
  expect_error(
    define_aoi(city_name = NULL, river_name = "MyRiver"),
    "Assertion on 'city_name' failed: Must be of type 'character'"
  )
  expect_error(
    define_aoi(city_name = "MyCity", river_name = NULL),
    "Assertion on 'river_name' failed: Must be of type 'character'"
  )
})

#' @srrstats {G2.9} A message is issued when CRS is not provided and a suitable
#'   UTM zone is auto-selected.
test_that("If `crs` is not specified, message is issued", {
  expect_message(
    with_mocked_bindings(
      get_osm_bb = \(...) {
        sf::st_bbox(c(xmin = 25.967, ymin = 44.334,
                      xmax = 26.226, ymax = 44.541),
                    crs = "EPSG:4326")
      },
      define_aoi(city_name = "MyCity", river_name = "MyRiver")
    ),
    "Using auto-selected UTM zone: EPSG:"
  )
})

#' @srrstats {G5.8} Edge test: NULL values are rejected for numeric parameters
test_that("NULL values are rejected for numeric buffer parameters", {
  expect_error(
    define_aoi(city_name = "MyCity", river_name = "MyRiver",
               network_buffer = NULL),
    "Assertion on 'network_buffer' failed: Must be of type 'numeric'"
  )
  expect_error(
    define_aoi(city_name = "MyCity", river_name = "MyRiver",
               buildings_buffer = NULL),
    "Assertion on 'buildings_buffer' failed: Must be of type 'numeric'"
  )
})

test_that("define_aoi() accepts units objects for buffer parameters", {
  with_mocked_bindings(
    get_osm_bb = \(...) sf::st_bbox(c(xmin = 25.967, ymin = 44.334,
                                      xmax = 26.226, ymax = 44.541),
                                    crs = "EPSG:4326"),
    {
      aoi <- define_aoi("MyCity", "MyRiver",
                        network_buffer   = units::set_units(3, "km"),
                        dem_buffer       = units::set_units(2500, "m"),
                        buildings_buffer = units::set_units(100, "m")) |>
        suppressMessages()
      expect_type(aoi$network_buffer,   "double")
      expect_equal(aoi$network_buffer,   3000)
      expect_equal(aoi$dem_buffer,       2500)
      expect_equal(aoi$buildings_buffer, 100)
    }
  )
})
