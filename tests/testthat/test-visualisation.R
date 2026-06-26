# Create test data
test_osm <- get_test_osm()
test_corridor <- sf::st_buffer(test_osm$river_centerline, 1000) |>
  sf::st_union()
test_valley <- sf::st_buffer(test_osm$river_centerline, 1500) |>
  sf::st_union()
test_segments <- c(
  sf::st_buffer(test_osm$river_centerline[1], 900) |> sf::st_union(),
  sf::st_buffer(test_osm$river_centerline[1], 800) |> sf::st_union()
)
delineation_object <- structure(
  list(
    streets = test_osm$streets,
    railways = test_osm$railways,
    river_centerline = test_osm$river_centerline,
    river_surface = test_osm$river_surface,
    corridor = test_corridor
  ),
  class = c("delineation", "list")
)

# Full delineation object with all optional layers and aoi for title
full_delineation_object <- structure(
  list(
    streets = test_osm$streets,
    railways = test_osm$railways,
    river_centerline = test_osm$river_centerline,
    river_surface = test_osm$river_surface,
    valley = test_valley,
    corridor = test_corridor,
    segments = test_segments,
    riverspace = test_corridor,
    aoi = list(city_name = "TestCity", river_name = "TestRiver",
               crs = sf::st_crs(test_osm$river_centerline))
  ),
  class = c("delineation", "list")
)

# Riverspace-only delineation object (no corridor)
riverspace_only_object <- structure(
  list(riverspace = test_corridor),
  class = c("delineation", "list")
)

base_layers <- c("streets", "railways", "river_centerline")

# Tests for plot.delineation

test_that("plot.delineation works with valid input", {
  withr::local_pdf(NULL)  # Avoid plot being saved as PDF in working directory
  expect_silent(plot(delineation_object))
})

test_that("plot.delineation throws error if input has no delineation layer", {
  delineation_object_nd <- delineation_object[base_layers]
  class(delineation_object_nd) <- c("delineation", "list")  # Restore class
  expect_error(plot(delineation_object_nd),
               "No delineation layers present in the delineation object.")
})

test_that("plot.delineation warns if one or more base layers are missing", {
  withr::local_pdf(NULL)  # Avoid plot being saved as PDF in working directory
  delineation_object_nb <-
    delineation_object[setdiff(names(delineation_object), base_layers)]
  class(delineation_object_nb) <- c("delineation", "list")  # Restore class
  expect_warning(plot(delineation_object_nb),
                 "Not all base layers found in the delineation object.")
})

test_that("plot.delineation throws error with input of wrong class", {
  delineation_object_unclass <- unclass(delineation_object)
  expect_error(plot.delineation(delineation_object_unclass),
               "The object is not of class 'delineation'")
})

test_that("plot.delineation uses riverspace to set extent when corridor is absent", {  # nolint
  withr::local_pdf(NULL)
  expect_warning(
    plot(riverspace_only_object),
    "Not all base layers found in the delineation object."
  )
})

test_that("plot.delineation renders all optional layers without error", {
  withr::local_pdf(NULL)
  expect_silent(plot(full_delineation_object))
})

test_that("plot.delineation renders title when aoi has city and river name", {
  withr::local_pdf(NULL)
  expect_silent(plot(full_delineation_object))
})

test_that("plot.delineation works with legend = FALSE", {
  withr::local_pdf(NULL)
  expect_silent(plot(delineation_object, legend = FALSE))
})

# geom_delineation tests

test_that("geom_delineation throws error for wrong class", {
  expect_error(geom_delineation(unclass(delineation_object)),
               "'x' must be an object of class 'delineation'.")
})

test_that("geom_delineation throws error when no delineation layers present", {
  empty_obj <- structure(
    list(streets = test_osm$streets),
    class = c("delineation", "list")
  )
  expect_error(geom_delineation(empty_obj),
               "No delineation layers present in the delineation object.")
})

test_that("geom_delineation returns a list of ggplot2 layers", {
  layers <- geom_delineation(delineation_object)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("geom_delineation works with extent = NULL", {
  layers <- geom_delineation(delineation_object, extent = NULL)
  expect_type(layers, "list")
  # No coord_sf appended when extent is NULL
  is_coord <- vapply(layers, \(l) inherits(l, "CoordSf"), logical (1))
  expect_false(any(is_coord))
})

test_that("geom_delineation warns when extent layer is absent", {
  expect_warning(
    geom_delineation(delineation_object, extent = "valley"),
    "Layer 'valley' not found; extent will not be restricted."
  )
})

test_that("geom_delineation renders all optional layers", {
  layers <- geom_delineation(full_delineation_object)
  expect_type(layers, "list")
  expect_true(length(layers) > 0)
})

test_that("geom_delineation works with legend = FALSE", {
  layers <- geom_delineation(delineation_object, legend = FALSE)
  expect_type(layers, "list")
})
