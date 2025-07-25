#' @srrstats {G5.10} An extended test that includes all the core algorithm of
#'   the package is included here. The test only run as part of the CI if a
#'   commit message includes the phrase 'run-extended'.
#' @srrstats {G5.11, G5.11a} The test downloads the required data. If any error
#'   arises due to access to web resources (HTTP the the error message), the
#'   test is skipped with an appropriate diagnostic message.
test_that("a full delineation run succeed, including data retrieval", {

  skip_if_not_extended()

  run_test <- \() {
    delineate(city_name = "Bucharest",
              river_name = "Dâmbovița",
              crs = 32635,
              network_buffer = 2500,
              buildings_buffer = 100,
              dem_buffer = 2500,
              corridor_init = "valley",
              corridor = TRUE,
              segments = TRUE,
              riverspace = TRUE) |>
      suppressMessages() |>
      suppressWarnings()
  }

  delineations <- tryCatch(
    {
      run_test()
    },
    error = function(e) {
      if (grepl("HTTP", conditionMessage(e), ignore.case = TRUE)) {
        skip(paste("Skipped due to HTTP error:", conditionMessage(e)))
      } else {
        stop(e)
      }
    }
  )

  expect_setequal(names(delineations),
                  c("valley", "corridor", "segments", "riverspace"))
  expect_true(all(vapply(
    delineations,
    \(x) inherits(x, c("sfc_POLYGON", "sfc_MULTIPOLYGON")),
    logical(1)
  )))
})
