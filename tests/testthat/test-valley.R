bb <- bucharest_osm$bb
asset_urls <- c(paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E026_00_DEM.tif"),
                paste0("s3://copernicus-dem-30m/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM/",
                       "Copernicus_DSM_COG_10_N44_00_E025_00_DEM.tif"))

test_that("STAC asset urls are correctly retrieved", {
  skip_on_ci()

  ep <- "https://earth-search.aws.element84.com/v1"
  col <- "cop-dem-glo-30"

  asset_urls_retrieved <- get_stac_asset_urls(bb, endpoint = ep,
                                              collection = col)
  expected_asset_urls <- asset_urls

  expect_equal(expected_asset_urls, asset_urls_retrieved)
})

test_that("load_raster correctly retrieve and merge local data", {

  write_local_raster <- function(fname, xmin, xmax, ymin, ymax) {
    rast <- terra::rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        res = 1, vals = 1, crs = "EPSG:4326")
    terra::writeRaster(rast, fname)
  }

  bbox <- sf::st_bbox(c(xmin = 1, xmax = 4, ymin = 1, ymax = 7),
                      crs = "EPSG:4326")
  # create local rasters with adjacent bboxes
  withr::with_file(list("r1.tif" = write_local_raster("r1.tif", 1, 4, 1, 4),
                        "r2.tif" = write_local_raster("r2.tif", 1, 4, 4, 7)), {
      rast <- load_raster(bbox, c("r1.tif", "r2.tif"))
      # all values should be 1
      expect_true(all(terra::values(rast) == 1))
      # 2 rasters with 3x3 pixels -> 18 pixels in total
      expect_length(terra::values(rast), 18)
      # expect_equal on the two terra::ext objects somehow fails
      expect_true(terra::ext(rast) == terra::ext(bbox))
    }
  )
})

test_that("load_raster correctly retrieve and merge remote data", {
  skip_on_ci()

  dem <- load_raster(bb, asset_urls)

  expect_equal(terra::crs(dem), terra::crs("EPSG:4326"))
  expect_equal(as.vector(terra::ext(dem)), as.vector(terra::ext(bb)),
               tolerance = 1.e-5)
})

test_that("valley polygon is correctly constructed", {
  dem <- terra::unwrap(bucharest_dem)
  river <- bucharest_osm$river_surface

  valley <- get_valley(dem, river)
  expected_valley_path <- testthat::test_path("testdata",
                                              "expected_valley.gpkg")
  expected_valley <- sf::st_read(expected_valley_path, quiet = TRUE)

  valley <- sf::st_set_precision(valley, 1e-06)
  expected_valley <- sf::st_set_precision(expected_valley, 1e-06)
  expect_true(sf::st_equals_exact(valley, expected_valley,
                                  par = 0, sparse = FALSE))
})
