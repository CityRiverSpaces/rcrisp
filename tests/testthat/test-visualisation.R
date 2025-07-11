delineation_object <- list(
  streets = bucharest_osm$streets,
  railways = bucharest_osm$railways,
  river_centerline = bucharest_osm$river_centerline,
  corridor = bucharest_dambovita$corridor
)
class(delineation_object) <- c("delineation", "list")

base_layers <- c("streets", "railways", "river_centerline")

test_that("plot.delineation works with valid input", {
  expect_silent(plot(delineation_object))  # Should not throw an error
})

test_that("plot.delineation throws error if input has no delineation layer", {
  delineation_object_nd <- delineation_object[base_layers]
  expect_error(plot(delineation_object_nd))
})

test_that("plot.delineation warns if one or more base layers are missing", {
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
