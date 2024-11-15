test_that("correct UTM zone is returend in the southern hemisphere", {
  # bbox for Chennai, India
  bbox <- sf::st_bbox(
    c(xmin = 80.11505, ymin = 12.92453, xmax = 80.27019, ymax = 13.08369),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone_epsg_sf(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32644)
})

test_that("correct UTM zone is returend in the northern hemisphere", {
  # bbox for Rejkjavik, Iceland
  bbox <- sf::st_bbox(
    c(xmin = -21.98383, ymin = 64.04040, xmax = -21.40200, ymax = 64.31537),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone_epsg_sf(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32627)
})
