river <- sf::st_sfc(sf::st_linestring(cbind(c(-6, 6), c(0, 0))))

test_that("Splitting the corridor works with a complex river geometry", {
  river_multilinestring <- sf::st_sfc(c(
    sf::st_linestring(cbind(c(-4, 6), c(0, 0))),
    sf::st_linestring(cbind(c(-6, -4), c(1, 0))),
    sf::st_linestring(cbind(c(-6, -4), c(-1, 0)))
  ))
  corridor <- sf::st_sfc(sf::st_polygon(list(cbind(c(-5, 5, 5, -5, -5),
                                                   c(1, 1, -1, -1, 1)))))
  edges <- get_corridor_edges(corridor, river_multilinestring)
  expect_length(edges, 2)
})

test_that("If the corridor cannot be split in two edges, an error is raised", {
  corridor <- sf::st_sfc(sf::st_polygon(list(cbind(c(-5, 5, 5, -5, -5),
                                                   c(2, 2, 1, 1, 2)))))
  expect_error(get_corridor_edges(corridor, river),
               "Cannot identify corridor edges")
})

test_that("Candidate segments boundaries are properly grouped and filtered", {
  e1 <- sf::st_linestring(cbind(c(-3, -3), c(-1, 1)))  # group 1 <--
  e2 <- sf::st_linestring(cbind(c(-3.1, -2.9), c(-1, 1)))  # group 1
  e3 <- sf::st_linestring(cbind(c(-2, -1.8), c(-1, 1)))  # group 2
  e4 <- sf::st_linestring(cbind(c(-2, -2), c(-1, 1)))  # group 2 <--
  e5 <- sf::st_linestring(cbind(c(-0.5, 0.5), c(-1, 1)))  # group 3 <--
  e6 <- sf::st_linestring(cbind(c(0.5, -0.5), c(-1, 1)))  # group 3
  e7 <- sf::st_linestring(cbind(c(1, 1), c(-1, 1)))  #  group 4 <--
  crossings <- sf::st_sfc(e1, e2, e3, e4, e5, e6, e7)
  expected <- sf::st_sfc(e1, e4, e5, e7)
  actual <- filter_clusters(crossings, river, eps = 0.2)
  expect_setequal(expected, actual)
})

test_that("Intersecting segment boundaries are correctly discarded", {
  e1 <- sf::st_linestring(cbind(c(-2, -1), c(1, -1)))
  e2 <- sf::st_linestring(cbind(c(1, 2), c(1, -1)))
  e3 <- sf::st_linestring(cbind(c(2, -2), c(1, -1)))
  e4 <- sf::st_linestring(cbind(c(3, 1), c(1, -1)))
  lines <- sf::st_sfc(e1, e2, e3, e4)
  corridor <- sf::st_polygon(list(cbind(c(-2, -2, 3, 3, -2),
                                        c(-1, 1, 1, -1, -1))))
  actual <- select_nonintersecting_lines(lines, corridor)
  expected <- sf::st_sfc(e1, e2)
  expect_setequal(actual, expected)
})

test_that("The longest intersecting segment boundaries are discarded first", {
  e1 <- sf::st_linestring(cbind(c(-2, -1), c(1, -1)))
  e2 <- sf::st_linestring(cbind(c(1, 2), c(1, -1)))
  e3 <- sf::st_linestring(cbind(c(0, -2), c(1, -1)))
  e4 <- sf::st_linestring(cbind(c(3, 1), c(1, -1)))
  lines <- sf::st_sfc(e1, e2, e3, e4)
  corridor <- sf::st_polygon(list(cbind(c(-2, -2, 3, 3, -2),
                                        c(-1, 1, 1, -1, -1))))
  actual <- select_nonintersecting_lines(lines, corridor)
  expected <- sf::st_sfc(e1, e2)
  expect_setequal(actual, expected)
})

test_that("Segment boundaries can intersect on the corridor edge", {
  e1 <- sf::st_linestring(cbind(c(-2, -1), c(1, -1)))
  e2 <- sf::st_linestring(cbind(c(1, 2), c(1, -1)))
  e3 <- sf::st_linestring(cbind(c(1, -1), c(1, -1)))
  lines <- sf::st_sfc(e1, e2, e3)
  corridor <- sf::st_polygon(list(cbind(c(-2, -2, 3, 3, -2),
                                        c(-1, 1, 1, -1, -1))))
  actual <- select_nonintersecting_lines(lines, corridor)
  expected <- sf::st_sfc(e1, e2, e3)
  expect_setequal(actual, expected)
})

test_that("The first segment boundary is discarded when length is equal", {
  e1 <- sf::st_linestring(cbind(c(-2, -1), c(1, -1)))
  e2 <- sf::st_linestring(cbind(c(1, 2), c(1, -1)))
  e3 <- sf::st_linestring(cbind(c(-1, -2), c(1, -1)))
  e4 <- sf::st_linestring(cbind(c(2, 1), c(1, -1)))
  lines <- sf::st_sfc(e1, e2, e3, e4)
  corridor <- sf::st_polygon(list(cbind(c(-2, -2, 3, 3, -2),
                                        c(-1, 1, 1, -1, -1))))
  actual <- select_nonintersecting_lines(lines, corridor)
  expected <- sf::st_sfc(e3, e4)
  expect_setequal(actual, expected)
})

# Correctness tests ----
#' @srrstats {G5.4, G5.4a} The correctness of DBSCAN clustering is tested
#'   against a simple, trivial case.
#' @srrstats {G5.5} Correctness tests for DBSCAN clustering are run with a fixed
#'   random seed.

crossings <- st_sfc(
  # Three crossings -> should be clustered
  st_linestring(matrix(c(0.0, 0.0, 0.0, 1.0), ncol = 2, byrow = TRUE)),
  st_linestring(matrix(c(0.1, 0.0, 0.1, 1.3), ncol = 2, byrow = TRUE)),
  st_linestring(matrix(c(0.2, 0.0, 0.2, 0.8), ncol = 2, byrow = TRUE)),

  # Two crossings -> should be clustered
  st_linestring(matrix(c(5.0, 0.0, 5.0, 1.1), ncol = 2, byrow = TRUE)),
  st_linestring(matrix(c(5.15, 0.0, 5.15, 0.9), ncol = 2, byrow = TRUE)),

  # Single crossing
  st_linestring(matrix(c(10.0, 0.0, 10.0, 1.5), ncol = 2, byrow = TRUE))
)
crossings_sf <- st_sf(id = 1:6, geometry = crossings)

river_sf <- st_sfc(
  st_linestring(matrix(c(-1.0, 0.3, 11.0, 0.3), ncol = 2, byrow = TRUE))
)

set.seed(1)
selected_crossings <- filter_clusters(segments_sf, river_sf, eps = 1) |>
  suppressWarnings()

test_that("Expected number of clusters is correct", {
  expect_equal(length(selected_crossings), 3)
})

test_that("The correct crossing segments are selected", {
  expect_equal(st_length(selected_crossings), c(0.8, 0.9, 1.5))
})
