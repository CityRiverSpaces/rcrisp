test_that("setting units works if x is unitless", {
  x <- 1
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should still be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_newshould now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
})

test_that("setting units works if x has unit", {
  x <- units::set_units(1, "m")
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should now be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_new should now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
  y_units <- units::set_units(y, "km")
  x_new <- set_units_like(x, y_units)  # x_new should now be converted to "km"
  expect_true(inherits(x_new, "units"))
  expect_equal(units::drop_units(x_new), 0.001)
})

test_that("correct UTM zone is returend in the southern hemisphere", {
  # bbox for Chennai, India
  bbox <- sf::st_bbox(
    c(xmin = 80.11505, ymin = 12.92453, xmax = 80.27019, ymax = 13.08369),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32644)
})

test_that("correct UTM zone is returend in the northern hemisphere", {
  # bbox for Rejkjavik, Iceland
  bbox <- sf::st_bbox(
    c(xmin = -21.98383, ymin = 64.04040, xmax = -21.40200, ymax = 64.31537),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32627)
})

test_that("both bbox and sf objects can be used to find UTM zone", {
  bbox <- sf::st_bbox(c(xmin = -20, ymin = 20, xmax = -21, ymax = 21),
                      crs = sf::st_crs(4326))
  geom <- sf::st_as_sf(sf::st_as_sfc(bbox))
  utm_epsg_bbox <- get_utm_zone(bbox)
  utm_epsg_geom <- get_utm_zone(geom)
  expect_equal(utm_epsg_bbox, utm_epsg_geom)
})
