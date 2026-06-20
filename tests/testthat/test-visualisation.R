test_osm <- get_test_osm()

test_corridor <- sf::st_buffer(test_osm$river_centerline, 1000) |>
  sf::st_union()

delineation_object <- list(
  streets = test_osm$streets,
  railways = test_osm$railways,
  river_centerline = test_osm$river_centerline,
  corridor = test_corridor
)
class(delineation_object) <- c("delineation", "list")

base_layers <- c("streets", "railways", "river_centerline")

test_that("plot.delineation works with valid input", {
  withr::local_pdf(NULL)
  expect_silent(plot(delineation_object))
})

test_that("plot.delineation throws error if input has no delineation layer", {
  delineation_object_nd <- delineation_object[base_layers]
  class(delineation_object_nd) <- c("delineation", "list")
  expect_error(plot(delineation_object_nd),
               "No delineation layers present in the delineation object.")
})

test_that("plot.delineation warns if one or more base layers are missing", {
  withr::local_pdf(NULL)
  delineation_object_nb <-
    delineation_object[setdiff(names(delineation_object), base_layers)]
  class(delineation_object_nb) <- c("delineation", "list")
  expect_warning(plot(delineation_object_nb),
                 "Not all base layers found in the delineation object.")
})

test_that("plot.delineation throws error with input of wrong class", {
  delineation_object_unclass <- unclass(delineation_object)
  expect_error(plot.delineation(delineation_object_unclass),
               "The object is not of class 'delineation'")
})
