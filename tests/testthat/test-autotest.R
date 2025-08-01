test_that("`autotest` checks pass", {
  skip_on_ci()
  skip_on_cran()

  # Turn off named tests with justification
  xf <- autotest::autotest_package("rcrisp")
  xf$note <- ""

  xf$test[xf$test_name == "rect_as_other"] <- FALSE
  xf$note[xf$test_name == "rect_as_other"] <-
    paste0("The `delineate_*` functions only expect rectangular input of ",
           "class `sf::sf`. Any other, non-spatial rectangular input will ",
           "normally raise an error.")

  xf$test[xf$test_name == "negate_logical"] <- FALSE
  xf$note[xf$test_name == "negate_logical"] <-
    paste0("The default or negated values of parameters identified by this ",
           "test correctly raise warnings or errors.")

  xf$test[xf$test_name == "return_desc_includes_class"] <- FALSE
  xf$note[xf$test_name == "return_desc_includes_class"] <-
    paste0("Return class is wrongly identified as ",
           "[simpleWarning, warning, condition]. ",
           "The function does raise a warning as normal behaviour, ",
           "but it also returns an object of correct class.")

  xf$test[xf$test_name == "vector_custom_class"] <- FALSE
  xf$note[xf$test_name == "vector_custom_class"] <-
    paste0("The function errors if the input object is of a class other than ",
           "the list of asserted classes. No other vector type than numeric ",
           "is accepted. This is normal function behavoiur.")

  xf$test[xf$test_name == "single_char_case"] <- FALSE
  xf$note[xf$test_name == "single_char_case"] <-
    paste0("Parameters identified with this test are either intentionally ",
           "case-dependent or false positives.")

  xf$test[xf$test_name == "par_is_demonstrated"] <- FALSE
  xf$note[xf$test_name == "par_is_demonstrated"] <-
    paste0("Parameters identified with this test are demonstrated in ",
           "examples. These are false positives.")

  xf$test[xf$test_name == "par_matches_docs"] <- FALSE
  xf$note[xf$test_name == "par_matches_docs"] <-
    paste0("Parameter classes are correctly specified in the documentation. ",
           "Parameters identified with this test are false positives.")

  xf$test[xf$test_name == "random_char_string"] <- FALSE
  xf$note[xf$test_name == "random_char_string"] <-
    paste0("The variables `key` and `value` are matched by `osmdata` to OSM ",
           "tags. As rcrisp cannot reliably maintain an updated list of OSM ",
           "tags, this test cannot be run.")

  xf$test[xf$test_name == "single_par_as_length_2"] <- FALSE
  xf$note[xf$test_name == "single_par_as_length_2"] <-
    paste0("The `value` parameter in `osmdata_as_sf()` does specify that the ",
           "length of the input vector can be longer than one both in the ",
           "parameter description and in the parameter size assertion.")

  # Run autotest with turned-off tests
  xt <- autotest::autotest_package("rcrisp", test = TRUE, test_data = xf) |>
    suppressWarnings()

  # Drop false-positive errors, warnings and messages from normal function calls
  xt <- xt |>
    dplyr::filter(!grepl("Loading data from cache directory", content),
                  !grepl("Saving data to cache directory", content),
                  !grepl("Corridor capping gives multiple polygons", content),
                  !grepl("successfully removed", content),
                  !grepl("missing, with no default", content),
                  # These named autotests were not removed above
                  !grepl("rect_as_other", test_name),
                  !grepl("par_matches_docs", test_name),
                  !grepl("single_par_as_length_2", test_name),
                  !grepl("par_is_demonstrated", test_name))
  if (nrow(xt) == 0) xt <- NULL

  # `autotest::expect_autotest_testdata(tf)` cannot be used because of the
  # false positives from normal function calls.
  expect_null(xt)
})
