# All functions using [`osmdata_as_sf()`] store data in a cache folder.
# In order not to mess up with the user cache directory, we setup a temporary
# cache folder only used for testing purposes. This is achieved via the
# [`temp_cache_dir()`] helper function, which should be called in each test.

test_that("City boundary of Bucharest is correctly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Bucharest"
  bb <- get_osm_bb(city_name)

  expect_message(bucharest_boundary <-
                   get_osm_city_boundary(bb, city_name,
                                         force_download = TRUE),
                 "City boundary found with 'boundary:administrative' tag.")
  expect_equal(as.numeric(bb), as.numeric(sf::st_bbox(bucharest_boundary)))
})

test_that("City boundary of Paris is returned without error", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  boundary <- get_osm_city_boundary(bb, city_name, crs, force_download = TRUE)
  expect_true(!sf::st_is_empty(boundary))
})

test_that("Wrong city name throws error", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  expect_error(get_osm_city_boundary("Buhcarest", force_download = TRUE))
})

test_that("OSM data for Bucharest is correctly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  bucharest <- get_osmdata("Bucharest", "Dâmbovița", force_download = TRUE)

  expect_length(bucharest, 4)
  expect_true(all(vapply(X = bucharest,
                         FUN = \(x) length(x) >= 1,
                         FUN.VALUE = logical(1))))
})

test_that("Multiple boundaries are correcly retreived", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Paris, France"
  bb <- get_osm_bb(city_name)
  crs <- get_utm_zone(bb)

  city_boundary <- get_osm_city_boundary(bb, city_name, crs, multiple = TRUE,
                                         force_download = TRUE)
  expect_gt(length(city_boundary), 1)
})

test_that("Queried datasets can be retrieved from the cache on new calls", {
  skip_on_ci()

  # setup cache directory
  cache_dir <- temp_cache_dir()

  bb <- get_osm_bb("Bucharest")

  # calling get_osm_railways should create a file in the cache folder
  expect_message(get_osm_railways(bb, force_download = TRUE),
                 "Saving data to cache directory")
  cached_filename <- list.files(cache_dir, pattern = "^osmdata_railway_rail")
  cached_filepath <- file.path(cache_dir, cached_filename)
  expect_true(file.exists(cached_filepath))

  # calling get_osm_railways again should read data from the cached file,
  # raising a warning that includes the path to the cached file as well
  expect_warning(get_osm_railways(bb, force_download = FALSE),
                 cached_filepath)
})

test_that("City boundary is retreived for alternative names", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_name <- "Köln"
  bb <- get_osm_bb(city_name)

  # test alternative names
  alternative_names <- c("Köln", "Cologne", "Colonia")
  for (name in alternative_names) {
    city_boundary <- get_osm_city_boundary(bb, name, force_download = TRUE)
    expect_equal(as.numeric(bb), as.numeric(sf::st_bbox(city_boundary)))
  }
})

test_that("River is consistently retreived with alternative names", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  river_name <- "Seine"
  bb_paris <- get_osm_bb("Paris, France")
  river <- get_osm_river(bb_paris, river_name, force_download = TRUE)
  bb_river <- sf::st_bbox(river$centerline)

  # test alternative names
  alternative_names <- c("La Seine", "Seine", "Senna")
  for (river_name in alternative_names) {
    river <- get_osm_river(bb_paris, river_name, force_download = TRUE)
    expect_equal(as.numeric(bb_river),
                 as.numeric(sf::st_bbox(river$centerline)))
  }
})

test_that("River retrieval raise error if no geometry is found", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  bb <- get_osm_bb("Paris, France")
  expect_error(get_osm_river(bb, "Thames", force_download = TRUE),
               "Thames")
})

test_that("All geometries retrieved from OSM are valid", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  bucharest_osm <- get_osmdata("Bucharest", "Dâmbovița", force_download = TRUE)

  expect_true(all(vapply(bucharest_osm[!names(bucharest_osm) %in% "bb"],
                         \(x) if (!inherits(x, "bbox")) all(sf::st_is_valid(x)),
                         logical(1))))
})

test_that("Both lines and multilines are retreived from river Dâmbovița", {
  skip_on_ci()

  # setup cache directory
  temp_cache_dir()

  city_names <- c("Bucharest", "Rio de Janeiro")
  river_names <- c("Dâmbovița", "Rio Guandu")

  for (i in seq_along(city_names)) {
    city_name <- city_names[i]
    river_name <- river_names[i]

    bb <- get_osm_bb(city_name)
    river <- get_osm_river(bb, river_name, force_download = TRUE)

    expect_true(length(river) > 0)
  }
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
