test_that("City boundary is correctly retreived", {
  skip_on_ci()
  bucharest_boundary_path <- testthat::test_path("testdata",
                                                 "city_boundary_Bucharest.gpkg")
  bucharest_boundary <- sf::st_read(bucharest_boundary_path, quiet = TRUE) |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  bucharest_boundary_osm <- get_osm_city_boundary("Bucharest") |>
    sf::st_geometry() |>
    sf::st_transform(4326)

  expect_equal(bucharest_boundary, bucharest_boundary_osm)
})

test_that("City boundary of Paris is correctly returned", {
  expect_no_error(get_osm_city_boundary("Paris, France"))
})

test_that("Wrong city name throws error", {
  expect_error(get_osm_city_boundary("Buhcarest"))
})
