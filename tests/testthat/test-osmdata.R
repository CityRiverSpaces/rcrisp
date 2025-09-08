# All functions using [`osmdata_as_sf()`] store and data in a cache folder and
# read data from there when already available. In order not to mess up with the
# user cache directory, we setup a temporary cache folder that already contains
# some datasets, only used for testing purposes. This is achieved via the
# [`temp_cache_dir()`] helper function, which should be called in each test that
# does not mock [`osmdata_as_sf()`].

# setup mock test dataset
bb_bucharest <- sf::st_bbox(c(xmin = 25.967,
                              ymin = 44.334,
                              xmax = 26.226,
                              ymax = 44.541),
                            crs = "EPSG:4326")
mock_river_lines_geom <- sf::st_sfc(
  sf::st_linestring(matrix(c(26.0, 26.1, 44.3, 44.4), ncol = 2)),
  sf::st_linestring(matrix(c(26.2, 26.3, 44.5, 44.6), ncol = 2)),
  sf::st_linestring(matrix(c(26.2, 26.3, 44.3, 44.4), ncol = 2)),
  sf::st_linestring(matrix(c(26.0, 26.1, 44.5, 44.6), ncol = 2)),
  crs = "EPSG:4326"
)
mock_river_lines <- sf::st_sf(
  name = c("Dâmbovița", "Dâmbovița", "Colentina", "Colentina"),
  geometry = mock_river_lines_geom
)
mock_river_polygons <- sf::st_buffer(mock_river_lines, 10)
mock_city_boundary_geom <- sf::st_as_sfc(bb_bucharest)
mock_city_boundary <- sf::st_sf(
  name = "Bucharest",
  `name:ro` = "București",
  admin_level = "4",
  geometry = mock_city_boundary_geom
)

test_that("OSM queries are stored to and retrieved from the cache", {

  # setup cache directory
  cache_dir <- temp_cache_dir()

  # test that content if first saved and then retrieved from the cache
  with_mocked_bindings(
    osmdata_query = function(...) "mock osmdata response",
    {
      # the first call to osmdata_as_sf saves data to cache
      expect_message(
        osmdata_as_sf("key", "value", bb_bucharest, force_download = TRUE),
        "Saving data to cache directory"
      )
      # check that file is in the cache
      cached_filename <- list.files(cache_dir, pattern = "^osmdata_key_value")
      cached_filepath <- file.path(cache_dir, cached_filename)
      expect_true(file.exists(cached_filepath))
      # subsequent calls read data from the file
      expect_warning(
        osmdata_as_sf("key", "value", bb_bucharest, force_download = FALSE),
        cached_filepath,
        fixed = TRUE
      )
    }
  )
})

test_that("OSM queries are always performed if force_download is set to TRUE", {

  # setup cache directory
  cache_dir <- temp_cache_dir()

  # test that cache data is not read if force_download = TRUE
  with_mocked_bindings(
    osmdata_query = function(...) "mock osmdata response",
    {
      # both calls to osmdata_as_sf save data to cache
      expect_message(
        osmdata_as_sf("key", "value", bb_bucharest, force_download = TRUE),
        "Saving data to cache directory"
      )
      expect_message(
        osmdata_as_sf("key", "value", bb_bucharest, force_download = TRUE),
        "Saving data to cache directory"
      )
    }
  )
})

#' @srrstats {G2.10} This test uses `sf::st_geometry()` to extract
#'   the geometry column from the `sf` object `mock_river_lines`. This is
#'   used when only geometry information is needed from that point onwards
#'   and all other attributes (i.e., columns) can be safely discarded. The
#'   object returned by `sf::st_geometry()` is a simple feature geometry list
#'   column of class `sfc`.
test_that("The correct OSM data elements are retrieved", {
  # setup cache directory, even though it shold not be used
  temp_cache_dir()

  # mock all functions that actually retrieve data from OSM. The only actual
  # data that is needed is the river centerline, which is used to setup the
  # areas of interest used for the network and building retrieval.
  river_centerline <- sf::st_geometry(mock_river_lines)[1]
  river <- list(centerline = river_centerline, surface = NULL)
  with_mocked_bindings(
    get_osm_bb = function(...) bb_bucharest,
    get_osm_river = function(...) river,
    get_osm_streets = function(...) NULL,
    get_osm_railways = function(...) NULL,
    get_osm_buildings = function(...) NULL,
    get_osm_city_boundary = function(...) NULL,
    {
      # By default, the bb, river, river suf
      osmdata_default <- get_osmdata("Bucharest",
                                     "Dâmbovița",
                                     force_download = TRUE)
      osmdata_nobound <- get_osmdata("Bucharest",
                                     "Dâmbovița",
                                     city_boundary = FALSE,
                                     force_download = TRUE)
      osmdata_network <- get_osmdata("Bucharest",
                                     "Dâmbovița",
                                     network_buffer = 3000,
                                     force_download = TRUE)
      osmdata_buildings <- get_osmdata("Bucharest",
                                       "Dâmbovița",
                                       buildings_buffer = 100,
                                       force_download = TRUE)
      osmdata_all <- get_osmdata("Bucharest",
                                 "Dâmbovița",
                                 network_buffer = 3000,
                                 buildings_buffer = 100,
                                 force_download = TRUE)

    }
  )

  # verify that the correct elements are returned
  expect_setequal(
    names(osmdata_default),
    c("bb", "river_centerline", "river_surface", "boundary")
  )
  expect_setequal(
    names(osmdata_nobound),
    c("bb", "river_centerline", "river_surface")
  )
  expect_setequal(
    names(osmdata_network),
    c(
      "bb", "river_centerline", "river_surface", "boundary", "aoi_network",
      "streets", "railways"
    )
  )
  expect_setequal(
    names(osmdata_buildings),
    c(
      "bb", "river_centerline", "river_surface", "boundary", "aoi_buildings",
      "buildings"
    )
  )
  expect_setequal(
    names(osmdata_all),
    c(
      "bb", "river_centerline", "river_surface", "boundary", "aoi_network",
      "streets", "railways", "aoi_buildings", "buildings"
    )
  )
})

test_that("City boundary of Bucharest is correctly retrieved", {
  with_mocked_bindings(
    osmdata_as_sf = function(...) list(osm_polygons = mock_city_boundary),
    expect_no_message(
      city_boundary <- get_osm_city_boundary(bb_bucharest, "Bucharest")
    )
  )
  expect_true(inherits(city_boundary, "sfc"))
})

#' @srrstats {G5.8, G5.8a} Edge test: if a value that leads to no data being
#'   retrieved, an error is raised.
test_that("Wrong city name raises an error", {
  expect_error(
    with_mocked_bindings(
      osmdata_as_sf = function(...) list(osm_polygons = mock_city_boundary),
      city_boundary <- get_osm_city_boundary(bb_bucharest, "Buhcarest")
    )
  )
})

test_that("Multiple boundaries are retreived when requested", {
  mock_city_boundary_multiple <- dplyr::bind_rows(mock_city_boundary,
                                                  mock_city_boundary)
  with_mocked_bindings(
    osmdata_as_sf = function(...) {
      list(osm_polygons = mock_city_boundary_multiple)
    },
    {
      expect_message(
        city_boundary_multiple <- get_osm_city_boundary(
          bb_bucharest, "Bucharest", multiple = TRUE
        ),
        "Multiple boundaries were found. Returning all."
      )
      expect_message(
        city_boundary_single <- get_osm_city_boundary(
          bb_bucharest, "București", multiple = FALSE
        ),
        "Multiple boundaries were found. Using the first one."
      )
    }
  )

  expect_equal(length(city_boundary_multiple), 2)
  expect_equal(length(city_boundary_single), 1)
})

test_that("City boundary is retrieved for alternative names", {
  with_mocked_bindings(
    osmdata_as_sf = function(...) list(osm_polygons = mock_city_boundary),
    {
      city_boundary_eng <- get_osm_city_boundary(bb_bucharest, "Bucharest")
      city_boundary_ro <- get_osm_city_boundary(bb_bucharest, "București")
    }
  )
  expect_equal(city_boundary_eng, city_boundary_ro)
})

#' @srrstats {G5.8, G5.8a} Edge test: if a value that leads to no data being
#'   retrieved, an error is raised.
test_that("River retrieval raise error if no river is found in the bb", {
  with_mocked_bindings(
    osmdata_as_sf = function(...) list(osm_lines = NULL),
    expect_error(
      get_osm_river(bb_bucharest, "Thames", force_download = TRUE),
      "No waterway geometries found"
    )
  )
})

#' @srrstats {G5.8, G5.8a} Edge test: if a value that leads to no data being
#'   retrieved, an error is raised.
test_that("River retrieval raise error if river is not found in the bb", {
  with_mocked_bindings(
    osmdata_as_sf = function(...) list(osm_lines = mock_river_lines),
    expect_error(
      get_osm_river(bb_bucharest, "Thames", force_download = TRUE),
      "Thames"
    )
  )
})

test_that("River lines and surface are properly set up", {
  with_mocked_bindings(
    osmdata_as_sf = function(...) {
      list(osm_lines = mock_river_lines, osm_polygons = mock_river_polygons)
    },
    river <- get_osm_river(bb_bucharest, "Dâmbovița", force_download = TRUE)
  )
  expect_setequal(names(river), c("centerline", "surface"))
  expect_true(sf::st_is(river$centerline, "MULTILINESTRING"))
  expect_true(sf::st_is(river$surface, "MULTIPOLYGON"))
})

test_that("If no railways are found, an empty sf object is returned", {
  crs <- sf::st_crs("EPSG:32632")
  aoi <- sf::st_bbox(c(xlim = 1, xmax = 2, ylim = 1, ymax = 2))
  mocked_osmdata_response <- list(osm_lines = NULL)
  with_mocked_bindings(osmdata_as_sf = function(...) mocked_osmdata_response, {
    railways <- get_osm_railways(aoi, crs = crs, force_download = FALSE)
  })
  expect_equal(nrow(railways), 0)
  expect_equal(sf::st_crs(railways), crs)
})

test_that("Partial matches of names are accounted for", {
  data <- data.frame(
    name = c("match", "xxx", "xxx", "xxx"),
    `name:xx` = c("xxx", "This is also a match", "xxx", "xxx"),
    `name:yy` = c("xxx", "xxx", "THIS IS ALSO A MATCH", "xxx"),
    discarded = c("xxx", "xxx", "xxx", "This is not a match")
  )
  res <- match_osm_name(data, "match")
  # Partial matches allowed, not case-sensitive. However, "name"
  # should be in the column name
  expect_equal(nrow(res), 3)
  # All columns are returned
  expect_equal(ncol(res), 4)
})
