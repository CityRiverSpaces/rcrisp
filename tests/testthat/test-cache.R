#' Fixture to setup temporary cache directory for testing
temp_cache_dir <- function(env = parent.frame()) {
  cache_dir <- file.path(tempdir(), "CRiSp-test-cache")

  # create temporary cache directory
  dir.create(cache_dir, recursive = TRUE)
  cache_dir <- normalizePath(cache_dir)
  withr::defer(unlink(cache_dir, recursive = TRUE), env)

  # copy test files to the cache directory
  source_dir <- testthat::test_path("testdata", "cache")
  filenames <- list.files(source_dir)
  file.copy(file.path(source_dir, filenames), cache_dir)

  # set environment variable
  current_value <- Sys.getenv("CRISP_CACHE_DIRECTORY", unset = NA)
  Sys.setenv(CRISP_CACHE_DIRECTORY = cache_dir)
  withr::defer(
    {
      if (is.na(current_value)) {
        Sys.unsetenv("CRISP_CACHE_DIRECTORY")
      } else {
        Sys.setenv(CRISP_CACHE_DIRECTORY = current_value)
      }
    },
    env
  )

  cache_dir
}

test_that("Cache directory is set via environmnent variable and created", {
  # create temporary folder
  root_dir <- withr::local_tempdir(pattern = "CRiSp-test-cache")
  cache_dir <- file.path(root_dir, ".cache", "CRiSp")
  withr::with_envvar(new = c("CRISP_CACHE_DIRECTORY" = cache_dir), {
    actual <- cache_directory()
    expected <- normalizePath(cache_dir)
    expect_equal(actual, expected)
    expect_true(dir.exists(actual))
  })
  # clean up temporary folder when test is complete
  withr::deferred_run()
})

test_that("A proper path is created to cache a OSM dataset", {
  cache_dir <- temp_cache_dir()
  key <- "highway"
  value <- c("primary", "secondary")
  bbox <- sf::st_bbox(c(xmin = 0, xmax = 1, ymin = -2, ymax = -1))
  actual <- get_osmdata_cache_filepath(key, value, bbox)
  expected <- file.path(cache_dir,
                        "osmdata_highway_primary_secondary_0_-2_1_-1.rds")
  expect_equal(actual, expected)
})

test_that("A proper path is created to cache a DEM dataset", {
  cache_dir <- temp_cache_dir()
  tile_urls <- c("https://www.mydem.org/tile1/tile1.tif",
                 "https://www.mydem.org/tile2/tile2.tif")
  bbox <- sf::st_bbox(c(xmin = 0, xmax = 1, ymin = -2, ymax = -1))
  actual <- get_dem_cache_filepath(tile_urls, bbox)
  expected <- file.path(cache_dir,
                        "dem_tile1_tile2_0_-2_1_-1.rds")
  expect_equal(actual, expected)
})

test_that("Loading/saving warnings can be suppressed via argument", {
  cache_dir <- temp_cache_dir()

  # check whether we can load test elements from the cache, with and without
  # warnings
  for (filename in list.files(cache_dir)) {
    expect_warning(
      read_data_from_cache(file.path(cache_dir, filename)),
      "Loading data from cache directory"
    )
    expect_no_warning(
      read_data_from_cache(file.path(cache_dir, filename), quiet = TRUE),
    )
  }

  # check whether we can write a test element to the cache, with and without
  # warnings
  x <- sf::st_sfc(sf::st_point(c(0, 1)))
  expect_message(
    write_data_to_cache(x, file.path(cache_dir, "point.rds")),
    "Saving data to cache directory"
  )
  expect_no_message(
    write_data_to_cache(x, file.path(cache_dir, "point.rds"), quiet = TRUE),
  )
})

test_that("Cached objects can be properly read", {
  cache_dir <- temp_cache_dir()

  # cached osmdata::osmdata_sf objects can be directly loaded
  filename <- list.files(cache_dir, pattern = "^osmdata*[.]rds$")[1]
  filepath <- file.path(cache_dir, filename)
  x <- read_data_from_cache(filepath, quiet = TRUE)
  expect_true(inherits(x, "osmdata_sf"))

  # cached terra::SpatRaster objects need to be unwrapped
  filename <- list.files(cache_dir, pattern = "^dem*[.]rds$")[1]
  filepath <- file.path(cache_dir, filename)
  x <- read_data_from_cache(filepath, unwrap = TRUE, quiet = TRUE)
  expect_true(inherits(x, "SpatRaster"))
})

test_that("Objects can be properly saved to cache", {
  cache_dir <- temp_cache_dir()

  # terra::SpatRaster objects need to wrapped
  x <- terra::rast(xmin = -174, xmax = -168, ymin = 45, ymax = 51, res = 1,
                   vals = 1, crs = "EPSG:4326")
  filepath <- file.path(cache_dir, "tmp-dem.rds")
  write_data_to_cache(x, filepath, wrap = TRUE, quiet = TRUE)
  expect_true(file.exists(filepath))

  # other objects (e.g. sf::sfc) can be serialized as they are
  x <- sf::st_sfc(sf::st_point(c(0, 1)))
  filepath <- file.path(cache_dir, "tmp-sfc.rds")
  write_data_to_cache(x, filepath, quiet = TRUE)
  expect_true(file.exists(filepath))
})
