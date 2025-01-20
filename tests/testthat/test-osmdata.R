test_that("City boundary of Bucharest is correctly retreived", {
  skip_on_ci()
  city_name <- "Bucharest"
  bb <- get_osm_bb(city_name)
  bucharest_boundary <- get_osm_city_boundary(bb, city_name)

  expect_equal(as.numeric(bb), as.numeric(sf::st_bbox(bucharest_boundary)))
})

test_that("City boundary of Paris is returned without error", {
  skip_on_ci()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  boundary <- get_osm_city_boundary(bb, city_name, crs)
  expect_true(!sf::st_is_empty(boundary))
})

test_that("Wrong city name throws error", {
  skip_on_ci()
  expect_error(get_osm_city_boundary("Buhcarest"))
})

test_that("OSM data for Bucharest is correctly retreived", {
  skip_on_ci()
  bb <- bucharest_osm$bb
  bucharest <- get_osmdata(bb, "Bucharest", "Dâmbovița")

  expect_length(bucharest, 6)
  expect_true(all(sapply(bucharest, function(x) length(x) >= 1)))
})

test_that("Multiple boundaries are correcly retreived", {
  skip_on_ci()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  expect_true(
    length(get_osm_city_boundary(bb, city_name, crs, multiple = TRUE)) > 1
  )
})
