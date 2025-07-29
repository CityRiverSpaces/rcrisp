test_that("`autotest` checks pass", {
  skip_on_ci()
  skip_on_cran()

  # Turn off named tests with justification
  xf <- autotest::autotest_package("rcrisp")
  xf$note <- ""

  xf$test[xf$test_name == "negate_logical"] <- FALSE
  xf$note[xf$test_name == "negate_logical"] <-
    "These tests are not applicable because ..."

  xf$test[xf$test_name == "rect_as_other"] <- FALSE
  xf$note[xf$test_name == "rect_as_other"] <-
    "These tests are not applicable because ..."

  xf$test[xf$test_name == "vector_custom_class"] <- FALSE
  xf$note[xf$test_name == "vector_custom_class"] <-
    "These tests are not applicable because ..."

  xf$test[xf$test_name == "single_char_case"] <- FALSE
  xf$note[xf$test_name == "single_char_case"] <-
    "Case-(in)sensitivity is handled properly. Diagnostics are false positives."

  xt <- autotest::autotest_package("rcrisp", test = TRUE, test_data = xf)

  # Remove false positives from normal function calls
  xt <- xt |>
    dplyr::filter(!grepl("Loading data from cache directory", content),
                  !grepl("Saving data to cache directory", content),
                  !grepl("successfully removed", content))

  expect_null(xt)
})
