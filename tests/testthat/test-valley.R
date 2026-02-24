# All functions using [`load_dem()`] store data in a cache folder.
# In order not to mess up with the user cache directory, we setup a temporary
# cache folder only used for testing purposes. This is achieved via the
# [`temp_cache_dir()`] helper function, which should be called in each test.

# Approximate bbox for Bucharest
bb <- c(xmin = 25.97, ymin = 44.33, xmax = 26.23, ymax = 44.54)
asset_urls <- c(paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM.tif"),
                paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM.tif"))

test_that("STAC asset urls are correctly retrieved", {
  skip_on_ci()
  skip_on_cran()

  ep <- "https://earth-search.aws.element84.com/v1"
  col <- "cop-dem-glo-30"

  asset_urls_retrieved <- tryCatch(
    {
      get_stac_asset_urls(bb, endpoint = ep, collection = col)
    },
    error = function(e) {
      if (grepl("HTTP", conditionMessage(e), ignore.case = TRUE)) {
        skip(paste("Skipped due to HTTP error:", conditionMessage(e)))
      } else {
        stop(e)
      }
    }
  )

  expected_asset_urls <- asset_urls

  expect_equal(expected_asset_urls, asset_urls_retrieved)
})

test_that("Download DEM data can be retrieved from the cache on new calls", {
  # setup cache directory
  cache_dir <- temp_cache_dir()
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-150, 150), c(150, -150))), crs = "EPSG:32601"
  )
  dem <- get_test_dem_valley(river)
  with_mocked_bindings(
    load_raster = function(...) {
      dem |> terra::project("EPSG:4326")
    },
    {
      # calling load_dem should create a file in the cache folder
      expect_message(load_dem(bb, asset_urls, force_download = TRUE),
                     "Saving data to cache directory")
      cached_filename <- list.files(cache_dir, pattern = "^dem_")
      cached_filepath <- file.path(cache_dir, cached_filename)
      expect_true(file.exists(cached_filepath))

      # calling load_dem again should read data from the cached file, raising a
      # warning that includes the path to the cached file as well
      expect_warning(load_dem(bb, asset_urls, force_download = FALSE),
                     cached_filepath, fixed = TRUE)
    }
  )
})

test_that("valley polygon is correctly constructed", {
  res <- 50
  crs <- "EPSG:32601"
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-10000, 10000), c(0, 0))),
    crs = crs
  )
  # Generate the DEM representing the valley around the river, using a fixed
  # slope.
  dem <- get_test_dem_valley(
    river, xmin = -10000, xmax = 10000, ymin = -4000, ymax = 4000, slope = 0.5,
    res = res
  )
  valley <- delineate_valley(dem, river)

  # We focus on the central part of the DEM, to be sure we neglect edge effects
  bbox <- sf::st_bbox(c(xmin = -5000, xmax = 5000, ymin = -5000, ymax = 5000))
  valley_crop <- sf::st_crop(valley, bbox)

  # We use a fixed slope for the valley, and calculate the characteristic cost
  # distance to extract the valley using a "mean" operator. Thus, the valley
  # edge is expected to lay at half the distance used to calculate the
  # characteristic cost distance, which is by default 2 km.
  river_buffer <- sf::st_buffer(river, 1000)  # 1 km
  valley_expected <- sf::st_crop(river_buffer, bbox)

  expect_true(
    sf::st_equals_exact(valley_crop, valley_expected, par = res, sparse = FALSE)
  )
})

#' @srrstats {G5.8} Edge test: if a value different from a set of
#'   allowed values is selected, an error is raised.
test_that("Unknown DEM source throws error", {
  expect_error(get_dem(bb, dem_source = "CATS")) # :)
})

#' @srrstats {G2.3b} Test that the value of dem_source passed to get_dem() is
#'   case insensitive.
test_that("DEM source is case insensitive", {
  with_mocked_bindings(
    load_dem = function(...) {
      terra::rast(matrix(1:4, nrow = 2), crs = "EPSG:4326")
    },
    get_stac_asset_urls = function(...) {
      asset_urls
    },
    {
      expect_error(get_dem(bb, dem_source = "stac"), NA)
      expect_error(get_dem(bb, dem_source = "STAC"), NA)
      expect_error(get_dem(bb, dem_source = "StAc"), NA)
    })
})

#' @srrstats {G5.8} Edge test: if input arguments are not consistent with each
#'    other, an error is raised.
test_that("Mismatch between DEM CRS and river CRS throws error", {
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-150, 150), c(150, -150))), crs = "EPSG:32601"
  )
  dem <- get_test_dem_valley(river)
  expect_error(
    delineate_valley(dem, st_transform(river, "EPSG:4326")),
    "DEM and river geometry should be in the same CRS"
  )
})

#' @srrstats {G5.8} Edge test: if input arguments are not consistent with each
#'    other, an error is raised.
test_that("Incorrect STAC endpoint and collection throws error", {
  expect_error(get_stac_asset_urls(bb, endpoint = "only endpoint"))
  expect_error(get_stac_asset_urls(bb, collection = "only collection"))
})

#' @srrstats {G5.8} Edge test: if a value different from a set of
#'   allowed values is selected, an error is raised.
test_that("Unimplemented function used to derive characteristic value throws
          error", {
            expect_error(get_cd_char(cd, "max"))
          })
