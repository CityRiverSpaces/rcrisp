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

test_that("Initial edges are identified if corridor exceeds AoI", {
  #        ____________
  #       |            |
  #       | north bank |
  #  _____|_ _ _ _ _ _ |_____
  # |     |____________|     | <- corridor
  # |_____|_ _ _ _ _ _ |_____|
  #       |            |
  #       | south bank |
  #       |____________|
  #
  # the dashed lines are the expected initial corridor edges
  corridor <- sf::st_as_sfc(sf::st_bbox(c(xmin = -2, xmax = 2,
                                          ymin = -0.5, ymax = 0.5)))
  north_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = 0, ymax = 1))
  south_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 0))
  regions <- c(sf::st_as_sfc(north_bank), sf::st_as_sfc(south_bank))
  edges_actual <- initial_edges(corridor, regions)
  edges_expected <- sf::st_sfc(
    sf::st_linestring(cbind(c(-1, 1), c(0.5, 0.5))),
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5)))
  )
  expect_setequal(edges_actual, edges_expected)
})

test_that("Initial edges are identified if AoI includes corridor", {
  #  ________________________
  # |                        |
  # |       north bank       |
  # |      _ _ _ _ _ _       |
  # |     |  corridor  |     |
  # |_____|____________|_____|
  # |     |            |     |
  # |     |_ _ _ _ _ _ |     |
  # |                        |
  # |       south bank       |
  # |________________________|
  #
  # the dashed lines are the expected initial corridor edges
  corridor <- sf::st_as_sfc(sf::st_bbox(c(xmin = -0.5, xmax = 0.5,
                                          ymin = -0.5, ymax = 0.5)))
  north_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = 0, ymax = 1))
  south_bank <- sf::st_bbox(c(xmin = -1, xmax = 1, ymin = -1, ymax = 0))
  regions <- c(sf::st_as_sfc(north_bank), sf::st_as_sfc(south_bank))
  edges_actual <- initial_edges(corridor, regions)
  edges_expected <- sf::st_sfc(
    sf::st_linestring(cbind(c(-0.5, -0.5, 0.5, 0.5), c(0, 0.5, 0.5, 0))),
    sf::st_linestring(cbind(c(0.5, 0.5, -0.5, -0.5), c(0, -0.5, -0.5, 0)))
  )
  expect_setequal(edges_actual, edges_expected)
})

test_that("Capping a corridor with method 'direct' properly closes a polygon", {
  edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
  edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
  edges <- sf::st_sfc(edge_1, edge_2)
  corridor <- cap_corridor(edges, method = "direct")
  pts_corridor <- sf::st_cast(corridor, "POINT")
  pts_edges <- sf::st_cast(edges, "POINT")
  # we verify that the corridor includes only points coming from the edges
  expect_setequal(pts_corridor, pts_edges)
})

test_that("Capping a corridor with 'shortest_path' uses network paths", {
  #    p2 -- p3
  #  /         \
  # p1          p4
  #  \         /
  #    p6 -- p5
  nodes <- sf::st_sfc(
    sf::st_point(c(-2, 0)),
    sf::st_point(c(-1, 1)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 0)),
    sf::st_point(c(1, -1)),
    sf::st_point(c(-1, -1))
  )
  edges <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(c(nodes[[1]], nodes[[2]])),
    sf::st_linestring(c(nodes[[2]], nodes[[3]])),
    sf::st_linestring(c(nodes[[3]], nodes[[4]])),
    sf::st_linestring(c(nodes[[4]], nodes[[5]])),
    sf::st_linestring(c(nodes[[5]], nodes[[6]])),
    sf::st_linestring(c(nodes[[6]], nodes[[1]]))
  ))
  edges$from <- c(1, 2, 3, 4, 5, 6)
  edges$to <- c(2, 3, 4, 5, 6, 1)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  corridor_edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
  corridor_edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
  corridor_edges <- sf::st_sfc(corridor_edge_1, corridor_edge_2)
  corridor <- cap_corridor(corridor_edges, method = "shortest-path",
                           network = network)
  pts_corridor <- sf::st_cast(corridor, "POINT")
  # we verify that the corridor includes all the nodes of the network
  expect_setequal(pts_corridor, nodes)
})