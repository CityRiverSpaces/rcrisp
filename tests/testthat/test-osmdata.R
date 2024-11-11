test_that("City boundary of Bucharest is correctly retreived", {
  skip_on_ci()
  bucharest_boundary <- bucharest$boundary |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  city_name <- "Bucharest"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone_epsg_bbox(bb) |> as.numeric()

  bucharest_boundary_osm <- get_osm_city_boundary(city_name, bb, crs) |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  expect_equal(bucharest_boundary, bucharest_boundary_osm)
})

test_that("City boundary of Paris is returned without error", {
  skip_on_ci()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone_epsg_bbox(bb) |> as.numeric()

  expect_no_error(get_osm_city_boundary(city_name, bb, crs))
})

test_that("Wrong city name throws error", {
  skip_on_ci()
  expect_error(get_osm_city_boundary("Buhcarest"))
})

test_that("OSM data for Bucharest is correctly retreived", {
  skip_on_ci()
  bucharest <- get_osmdata("Bucharest", "Dâmbovița", buffer = 2000)

  expect_length(bucharest, 6)
  expect_true(all(sapply(bucharest, function(x) length(x) >= 1)))
})

test_that("Multiple boundaries are correcly retreived", {
  skip_on_ci()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone_epsg_bbox(bb) |> as.numeric()

  expect_true(length(get_osm_city_boundary(city_name, bb, crs, multiple = TRUE)) > 1)
})
