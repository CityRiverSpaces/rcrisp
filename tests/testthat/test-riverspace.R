test_that("The riverspace of Dâmbovița within 100m is correctly returned", {
  skip_on_ci()
  skip_on_cran()
  bucharest_osm <- get_osm_example_data()
  riverspace_actual <- delineate_riverspace(
    river = bucharest_osm$river_surface,
    occluders = bucharest_osm$buildings,
    ray_length = 100
  )
  actual_surface <- sf::st_area(riverspace_actual)
  # A tolerance of 100,000 m^2 is used to account for changes in input data
  # (buildings added or removed in OSM)
  expected_surface <- units::set_units(8373547, "m^2")
  expect_lt(abs(expected_surface - actual_surface),
            units::set_units(1e05, "m^2"))
})

test_that(
  "The area of a riverspace is smaller than an unoccluded buffer and larger
  than the water surface",
  {
    test_osmdata <- get_test_osmdata()
    river_surface <- test_osmdata$river_surface
    buildings <- test_osmdata$buildings
    riverspace <- delineate_riverspace(
      river = river_surface, occluders = buildings, ray_length = 100
    )
    actual_surface <- sf::st_area(riverspace)
    river_surface_buffer <- sf::st_buffer(river_surface, 100)
    river_surface_buffer_area <- sf::st_area(river_surface_buffer)
    river_surface_area <- sf::st_area(river_surface)
    expect_lt(actual_surface, river_surface_buffer_area)
    expect_gt(actual_surface, river_surface_area)
  }
)

#' @srrstats {SP6.1a} Geographic (lat/lon) input to `delineate_riverspace()`
#'   yields inaccurate results because all riverspace geometry operations
#'   assume Cartesian (projected) coordinates. The function therefore raises an
#'   informative error when geographic CRS input is supplied.
test_that("delineate_riverspace() raises an error for geographic CRS input", {
  river_surface <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(26.07, 26.07, 26.09, 26.09, 26.07),
      c(44.435, 44.445, 44.445, 44.435, 44.435)
    ))),
    crs = 4326
  )
  occluders <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(26.071, 26.071, 26.072, 26.072, 26.071),
      c(44.436, 44.437, 44.437, 44.436, 44.436)
    ))),
    crs = 4326
  )
  expect_error(
    delineate_riverspace(river_surface, occluders),
    "The input CRS is geographic"
  )
})
