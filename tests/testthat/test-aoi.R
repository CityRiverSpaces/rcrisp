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
