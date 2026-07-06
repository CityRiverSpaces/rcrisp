#' @srrstats {G5.8} Edge test: in the following two tests, an error is raised
#'   if the dimension of the input parameters does not fit the requirements.
#' @noRd
#' NULL

test_that("Parameters can be set for only one city or river at a time", {
  expect_error(define_aoi(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
  expect_error(define_aoi("Bucharest", c("Dâmbovița", "Colentina")),
               "Assertion on 'river_name' failed: Must have length 1")
})

test_that("Buffer parametes are of length 1", {
  expect_error(define_aoi("Bucharest", "Dâmbovița", network_buffer = c(1, 2)),
               "Assertion on 'network_buffer' failed: Must have length 1")
  expect_error(define_aoi("Bucharest", "Dâmbovița", dem_buffer = c(1, 2)),
               "Assertion on 'dem_buffer' failed: Must have length 1")
  expect_error(define_aoi("Bucharest", "Dâmbovița", buildings_buffer = c(1, 2)),
               "Assertion on 'buildings_buffer' failed: Must have length 1")
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

bb_bucharest <- sf::st_bbox(c(xmin = 25.967,
                              ymin = 44.334,
                              xmax = 26.226,
                              ymax = 44.541),
                            crs = "EPSG:4326")

test_that("If no CRS is given, then appropriate UTM zone is returned", {
  with_mocked_bindings(
    get_osm_bb = function(...) bb_bucharest,
    {
      aoi <- define_aoi("Bucharest", "Dâmbovița")
    }
  )
  expect_equal(aoi$crs, sf::st_crs(32635))
})

test_that("Given CRS is transformed to appropriate UTM zone", {
  with_mocked_bindings(
    get_osm_bb = function(...) bb_bucharest,
    {
      aoi <- define_aoi("Bucharest", "Dâmbovița", crs = "EPSG:32635")
    }
  )
  expect_equal(aoi$crs, sf::st_crs(32635))
})

test_that("AOI object returns correct list of items", {
  with_mocked_bindings(
    get_osm_bb = function(...) bb_bucharest,
    {
      aoi <- define_aoi("Bucharest", "Dâmbovița")
    }
  )
  expect_setequal(names(aoi), c("city_name",
                                "river_name",
                                "bb",
                                "crs",
                                "network_buffer",
                                "dem_buffer",
                                "buildings_buffer"))
})
