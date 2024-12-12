if (Sys.getenv("NOT_CRAN") != "true") {
  dir.create("fixtures", showWarnings = FALSE)
  file.copy("tests/testthat/fixtures/expected_valley.gpkg",
            "fixtures/expected_valley.gpkg")
}
