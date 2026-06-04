test_that("When buffers are not specified, their default value is used", {
  expect_message(define_aoi(city_name = "MyCity",
                            river_name = "MyRiver") |>
                   suppressWarnings(),
                 paste0("The default `network_buffer` of 3000 m ",
                        "is used for corridor delineation."))
  expect_message(define_aoi(city_name = "MyCity",
                            river_name = "MyRiver") |>
                   suppressWarnings(),
                 paste0("The default `buildings_buffer` of 100 m ",
                        "is used for riverspace delineation."))
})

#' @srrstats {G5.8} Edge test: an error is raised if the dimension of the input
#'   parameters does not fit the requirements.
test_that("Parameters can be set for only one city at a time", {
  expect_error(define_aoi(c("Bucharest", "Cluj-Napoca"), "Dâmbovița"),
               "Assertion on 'city_name' failed: Must have length 1")
})
