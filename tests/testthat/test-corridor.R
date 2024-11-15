test_that("river_buffer parameters can be configured via initial_corridor", {
  river <- bucharest$river_centerline
  actual <- initial_corridor(river, buffer = 1)
  expected <- river_buffer(river, buffer = 1)
  expect_setequal(actual, expected)
})

test_that("River buffer properly implements a buffer function", {
  river <- bucharest$river_centerline
  actual <- river_buffer(river, buffer = 0.5)
  expected <- sf::st_buffer(river, 0.5)
  expect_setequal(actual, expected)
})

test_that("River buffer can trim to the region of interest", {
  river <- bucharest$river_centerline
  bbox <- sf::st_bbox(bucharest$boundary)
  actual <- river_buffer(river, buffer = 0.5, bbox = bbox)
  river_buffer <- sf::st_buffer(river, 0.5)
  overlap_matrix <- sf::st_overlaps(river_buffer, actual, sparse = FALSE)
  expect_equal(dim(overlap_matrix), c(1, 1))
  expect_true(overlap_matrix[1, 1])
  actual_bbox <- sf::st_bbox(actual)
  expect_true(all(actual_bbox[c("xmin", "ymin")] >= bbox[c("xmin", "ymin")]))
  expect_true(all(actual_bbox[c("xmax", "ymax")] <= bbox[c("xmax", "ymax")]))
})

test_that("Endpoints are found for two point intersections with bbox", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  bbox <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 1))
  actual <- corridor_end_points(river, bbox)
  expected <- sf::st_sfc(sf::st_point(c(-1, 0)), sf::st_point(c(0, -1)))
  expect_setequal(actual, expected)
})

test_that("Endpoints are found even for multiple intersections with bbox", {
  skip("river crossing AoI in multiple points not yet implemented")
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0),
                                              c(1, 1, -2))))
  bbox <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 1))
  actual <- corridor_end_points(river, bbox)
  expected <- sf::st_sfc(sf::st_point(c(0, 1)), sf::st_point(c(0, -1)))
  expect_setequal(actual, expected)
})

test_that("Splitting an AoI by a crossing river gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  bbox <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 1))
  aoi_split <- split_aoi(bbox, river)
  expect_equal(length(aoi_split), 2)
})

test_that("Splitting an AoI by a more complex river still gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 2, 2, -2),
                                              c(0.5, 0.5, -0.75, -0.75))))
  bbox <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 1))
  aoi_split <- split_aoi(bbox, river)
  expect_equal(length(aoi_split), 2)
})

test_that("Splitting an AoI by a river works with real data", {
  bbox <- sf::st_bbox(bucharest$boundary)
  river <- bucharest$river_centerline
  aoi_split <- split_aoi(bbox, river)
  expect_equal(length(aoi_split), 2)
})
