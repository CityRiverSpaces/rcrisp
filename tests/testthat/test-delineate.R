test_that("Segmentation without corridor raises error", {
  skip_on_ci()
  expect_error(delineate("Bucharest", "Dâmbovița",
                         corridor = FALSE, segment = TRUE),
               "Segmentation requires corridor delineation.")
})

test_that("Default network buffer is used with message", {
  skip_on_ci()
  expect_message(suppressWarnings(delineate("Bucharest", "Dâmbovița")),
                 paste("The default `network_buffer` of 3000 m is used",
                       "for corridor delineation."))
})

test_that("Default buildings buffer is used for riverspace delineation
          with message", {
            skip_on_ci()
            expect_message(suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                                      riverspace = TRUE)),
                           paste("The default `buildings_buffer` of 100 m is",
                                 "used for riverspace delineation."))
          })

test_that("Valley is returned correctly", {
  skip_on_ci()

  valley <- suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                       valley = TRUE,
                                       corridor = TRUE,
                                       segments = TRUE,
                                       riverspace = TRUE))$valley |>
    sf::st_set_precision(1e-06)

  expected_valley_path <- testthat::test_path("testdata",
                                              "expected_valley.gpkg")
  expected_valley <- sf::st_read(expected_valley_path, quiet = TRUE) |>
    sf::st_geometry() |>
    sf::st_set_precision(1e-06)

  expect_true(sf::st_equals_exact(valley, expected_valley,
                                  par = 0, sparse = FALSE))

})

test_that("Throw error if valley is requested with wrong initial_method", {
  skip_on_ci()
  expect_error(suppressWarnings(delineate("Bucharest", "Dâmbovița",
                                          initial_method = "buffer",
                                          valley = TRUE)),
               paste0('The valley can only be returned if `initial_method` is,
                      `"valley"`'))
})
