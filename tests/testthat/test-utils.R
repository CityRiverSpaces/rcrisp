#' @srrstats {G2.10} Several tests in this test module use `sf::st_geometry()`
#'   to extract the geometry column from the either
#'   `bucharest_osm$river_centerline` or `bucharest_osm$river_surface`. This is
#'   used when only geometry information is needed from that point onwards and
#'   all other attributes (i.e., columns) can be safely discarded. The object
#'   returned by `sf::st_geometry()` is a simple feature geometry list column of
#'   class `sfc`.
#' @noRd
NULL

test_that("setting units works if x is unitless", {
  x <- 1
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should still be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_newshould now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
})

test_that("setting units works if x has unit", {
  x <- units::set_units(1, "m")
  y <- 2
  x_new <- set_units_like(x, y)  # x_new should now be unitless
  expect_true(!inherits(x_new, "units"))
  y_units <- units::set_units(y, "m")
  x_new <- set_units_like(x, y_units)  # x_new should now have "m" unit
  expect_true(inherits(x_new, "units"))
  expect_equal(units(x_new), units(y_units))
  y_units <- units::set_units(y, "km")
  x_new <- set_units_like(x, y_units)  # x_new should now be converted to "km"
  expect_true(inherits(x_new, "units"))
  expect_equal(units::drop_units(x_new), 0.001)
})

test_that("correct UTM zone is returend in the southern hemisphere", {
  # bbox for Chennai, India
  bbox <- sf::st_bbox(
    c(xmin = 80.11505, ymin = 12.92453, xmax = 80.27019, ymax = 13.08369),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32644)
})

test_that("correct UTM zone is returend in the northern hemisphere", {
  # bbox for Rejkjavik, Iceland
  bbox <- sf::st_bbox(
    c(xmin = -21.98383, ymin = 64.04040, xmax = -21.40200, ymax = 64.31537),
    crs = sf::st_crs(4326)
  )
  utm_epsg <- get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox)))
  expect_equal(utm_epsg, 32627)
})

#' @srrstats {SP6.2} The software works mostly in projected coordinates. By
#'   default we work with UTM zones, which are defined only outside polar
#'   regions. We make sure that errors are raised if polar regions are
#'   considered.
#' @srrstats {G5.8, G5.8d} Edge test: input outside the expected validity ranges
#'   raise an error.
test_that("an error is raised for latitudes outside the validity range", {
  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 83, xmax = 1, ymax = 84.1),
    crs = sf::st_crs(4326)
  )
  expect_error(get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox))))
  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = -80.5, xmax = 1, ymax = 75),
    crs = sf::st_crs(4326)
  )
  expect_error(get_utm_zone(sf::st_as_sf(sf::st_as_sfc(bbox))))
})

test_that("both bbox and sf objects can be used to find UTM zone", {
  bbox <- sf::st_bbox(c(xmin = -20, ymin = 20, xmax = -21, ymax = 21),
                      crs = sf::st_crs(4326))
  geom <- sf::st_as_sf(sf::st_as_sfc(bbox))
  utm_epsg_bbox <- get_utm_zone(bbox)
  utm_epsg_geom <- get_utm_zone(geom)
  expect_equal(utm_epsg_bbox, utm_epsg_geom)
})

test_that("a matrix is correctly converted to a bbox", {
  bb <- matrix(data = c(0, 1, 2, 3),
               nrow = 2,
               ncol = 2,
               dimnames = list(c("x", "y"), c("min", "max")))
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a vector is correctly converted to a bbox", {
  bb <- c(0, 1, 2, 3)
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a sf object is correctly converted to a bbox", {
  linestring <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 1, 2, 3), ncol = 2, byrow = TRUE))
  )
  bbox <- as_bbox(linestring)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(4326))
})

test_that("a bbox object does not change class", {
  crs <- 3285
  bb <- sf::st_bbox(c(xmin = 0, ymin = 1, xmax = 2, ymax = 3), crs = crs)
  bbox <- as_bbox(bb)
  expect_true(inherits(bbox, "bbox"))
  expect_true(all(as.vector(bbox) == c(0, 1, 2, 3)))
  expect_equal(sf::st_crs(bbox), sf::st_crs(crs))
})

test_that("buffering a bbox properly enlarge the area of interest", {
  # bbox in UTM zone 2N
  x <- c(xmin = 263554, xmax = 736446, ymin = 4987330, ymax = 5654109)
  bbox_utm2n <- sf::st_bbox(x, crs = "EPSG:32602")

  bbox_buffer_actual <- buffer(bbox_utm2n, 1000)

  y <- c(x[c("xmin", "ymin")] - 1000, x[c("xmax", "ymax")] + 1000)
  bbox_buffer_expected <- sf::st_bbox(y, crs = "EPSG:32602")

  expect_equal(bbox_buffer_actual, bbox_buffer_expected)
})

test_that("buffering a bbox does not change its CRS", {
  # bbox in WGS 84
  x <- c(xmin = -174, xmax = -168, ymin = 45, ymax = 51)
  bbox_wgs84 <- sf::st_bbox(x, crs = "EPSG:4326")

  bbox_buffer <- buffer(bbox_wgs84, 1000)

  crs_expected <- sf::st_crs(bbox_wgs84)
  crs_actual <- sf::st_crs(bbox_buffer)
  expect_equal(crs_actual, crs_expected)
})

test_that("Buffer also works without a CRS", {
  x <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0), c(0, -2))))
  x_buff <- buffer(x, 1)
  expect_true(is.na(sf::st_crs(x_buff)))
  expect_true(inherits(x_buff, c("sfc_POLYGON", "sfc_MULTIPOLYGON")))
})

test_that("River buffer implements a buffer function", {
  river <- bucharest_osm$river_centerline |> sf::st_geometry()
  actual <- river_buffer(river, buffer_distance = 0.5)
  expected <- sf::st_buffer(river, 0.5)
  expect_setequal(actual, expected)
})

test_that("River buffer can trim to the region of interest", {
  river <- bucharest_osm$river_centerline
  bbox <- sf::st_bbox(bucharest_osm$boundary)
  actual <- river_buffer(river, buffer_distance = 10, bbox = bbox)
  river_buffer <- sf::st_buffer(river, 10)
  # set precision to bypass numerical issues
  actual <- sf::st_set_precision(actual, 1.e-3)
  river_buffer <- sf::st_set_precision(river_buffer, 1.e-3)
  covers <- sf::st_covers(river_buffer, actual, sparse = FALSE)
  expect_true(covers)
})

#' @srrstats {G5.8} Edge test: if a value different from a set of
#'   allowed values is selected, an error is raised.
test_that("River buffer throws error if wrong 'side' value is provided", {
  river <- bucharest_osm$river_centerline
  bbox <- sf::st_bbox(bucharest_osm$boundary)
  expect_error(
    river_buffer(river, buffer_distance = 10, bbox = bbox, side = "wrong"),
    "If specified, 'side' should be either 'right' or 'left'"
  )
})

#' @srrstats {G2.4, G2.4a} Explicit conversion to integer with `as.integer()`
#'   used to test `reproject()` with different ways of providing CRS input.
test_that("reproject works with raster data", {
  # raster in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- terra::rast(xmin = -174, xmax = -168, ymin = 45, ymax = 51, res = 1,
                   vals = 1, crs = "EPSG:4326")

  # reproject with integer/numeric (EPSG code)
  x_repr_num <- reproject(x, 32602)
  x_repr_int <- reproject(x, as.integer(32602))

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  # reproject with sf::crs object
  x_repr_crs <- reproject(x, sf::st_crs(32602))

  crs_expected <- terra::crs("EPSG:32602")
  crs_actual_num <- terra::crs(x_repr_num)
  expect_equal(crs_actual_num, crs_expected)
  crs_actual_int <- terra::crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- terra::crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
  crs_actual_crs <- terra::crs(x_repr_crs)
  expect_equal(crs_actual_crs, crs_expected)
})

test_that("reproject works with vector data", {
  # polygon in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- sf::st_linestring(cbind(c(-174, -174, -168, -168, -174),
                               c(45, 51, 51, 45, 45)))
  x <- sf::st_polygon(list(x))
  x <- sf::st_sfc(x, crs = "EPSG:4326")

  # reproject with integer (EPSG code)
  x_repr_int <- reproject(x, 32602)

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  crs_expected <- sf::st_crs("EPSG:32602")
  crs_actual_int <- sf::st_crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- sf::st_crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
})

test_that("reproject works with bbox", {
  # bbox in UTM zone 2 (lon between -174 and -168 deg), northern emisphere
  x <- c(xmin = -174, xmax = -168, ymin = 45, ymax = 51)
  x <- sf::st_bbox(x, crs = "EPSG:4326")

  # reproject with integer (EPSG code)
  x_repr_int <- reproject(x, 32602)

  # reproject with string
  x_repr_str <- reproject(x, "EPSG:32602")

  crs_expected <- sf::st_crs("EPSG:32602")
  crs_actual_int <- sf::st_crs(x_repr_int)
  expect_equal(crs_actual_int, crs_expected)
  crs_actual_str <- sf::st_crs(x_repr_str)
  expect_equal(crs_actual_str, crs_expected)
})

#' @srrstats {G5.8, G5.8b} Edge test: giving as input a value of wrong type
#'   raises an error is raised.
test_that("reproject does not work with objects of unknown type", {
  expect_error(reproject(1, 4326),
               "Must inherit from class 'SpatRaster'/'sf'/'sfc'")
})

#' @srrstats {SP6.0} This test checks that the original coordinates of a raster
#'   can be recovered after reprojection within a reasonable numeric tolerance.
test_that("the original coorindates of a raster can be recovered after
          reprojection within reasonable numeric tolerance", {
            # raster in WGS84
            x <- terra::rast(xmin = -174, xmax = -168, ymin = 45, ymax = 51,
                             res = 1, vals = 1, crs = "EPSG:4326")

            # reproject to UTM zone 2
            x_repr <- reproject(x, "EPSG:32602")

            # recover original CRS
            x_recovered <- reproject(x_repr, "EPSG:4326")

            # check that the original coordinates are recovered
            # within a tolerance of 1 (the size of a grid cell)
            expect_true(all(abs(terra::xmin(x) - terra::xmin(x_recovered)) < 1))
          })

#' @srrstats {SP6.0} This test checks that the original coordinates of a vector
#'   can be recovered after reprojection within a reasonable numeric tolerance.
test_that("the original coorindates of vector data can be recovered after
          reprojection within reasonable numeric tolerance", {
            # polygon in WGS84
            x <- sf::st_polygon(list(cbind(c(-174, -174, -168, -168, -174),
                                           c(45, 51, 51, 45, 45))))
            x <- sf::st_sfc(x, crs = "EPSG:4326")

            # reproject to UTM zone 2
            x_repr <- reproject(x, "EPSG:32602")

            # recover original CRS
            x_recovered <- reproject(x_repr, "EPSG:4326")

            # check that the original coordinates are recovered
            expect_true(
              all(abs(sf::st_coordinates(x) -
                        sf::st_coordinates(x_recovered)) < 1e-06)
            )
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
      rast <- load_raster(c("r1.tif", "r2.tif"), bbox = bbox)
      # all values should be 1
      expect_true(all(terra::values(rast) == 1))
      # 2 rasters with 3x3 pixels -> 18 pixels in total
      expect_length(terra::values(rast), 18)
      # expect_equal on the two terra::ext objects somehow fails
      expect_true(terra::ext(rast) == terra::ext(bbox))
    }
  )
})

test_that("River centerline and surface are combined without overlap", {
  l_centerline <- sf::st_length(bucharest_osm$river_centerline)
  l_surface <- bucharest_osm$river_surface |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_length()
  l_overlap <- bucharest_osm$river_centerline |>
    sf::st_intersection(bucharest_osm$river_surface) |>
    sf::st_length()
  l_combined_expected <- l_centerline + l_surface - l_overlap
  l_combined_actual <-
    combine_river_features(sf::st_geometry(bucharest_osm$river_centerline),
                           sf::st_geometry(bucharest_osm$river_surface)) |>
    sf::st_length()
  expect_equal(l_combined_actual, l_combined_expected)
})

test_that("When river surface is not available,
  river centerline is used with warning",
          {
            expect_warning(
              combine_river_features(bucharest_osm$river_centerline, NULL),
              regexp = "*Calculating viewpoints along river centerline.*"
            )
          })

test_that(
  "When both river centerlin and river surface are used for setting viewpoints,
  message is returned",
  {
    expect_message(
      combine_river_features(
        bucharest_osm$river_centerline |> sf::st_geometry(),
        bucharest_osm$river_surface |> sf::st_geometry()
      ),
      "*Calculating viewpoints from both river edge and river centerline.*"
    )
  }
)

test_that("Invalid geometry input produces message", {
  invalid_geom <-
    st_polygon(list(rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0))))
  expect_message(
    check_invalid_geometry(invalid_geom),
    "Invalid geometries detected! Fixing them..."
  )
})
