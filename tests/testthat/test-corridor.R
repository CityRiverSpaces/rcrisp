test_that("Build river network works with multiple linestring features", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expect_setequal(river, actual_edges)
})

test_that("Build river network does not simplify loops", {
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-2, -1), c(0, 0))),
    sf::st_linestring(cbind(c(-1, 0, 0), c(0, 0, -1))),
    sf::st_linestring(cbind(c(0, 0), c(-1, -2))),
    sf::st_linestring(cbind(c(-1, -1, 0), c(0, -1, -1)))
  )
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expect_setequal(river, actual_edges)
})

test_that("Build river network also works with multilinestrings", {
  river <- sf::st_sfc(c(
    sf::st_linestring(cbind(c(-2, -1), c(0, 0))),
    sf::st_linestring(cbind(c(-1, 0, 0), c(0, 0, -1))),
    sf::st_linestring(cbind(c(-1, -1, 0), c(0, -1, -1))),
    sf::st_linestring(cbind(c(0, 0), c(-1, -2)))
  ))
  river_network <- build_river_network(river)
  expect_true(inherits(river_network, "sfnetwork"))
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expected_edges <- sf::st_cast(river, "LINESTRING")
  expect_setequal(expected_edges, actual_edges)
})

test_that("Build river network only select longest segment within AoI", {
  river <- sf::st_sfc(
    sf::st_linestring(cbind(c(-2, 0, 0, -1), c(0, 0, -1, -1)))
  )
  bbox <- sf::st_bbox(c(xmin = -1.5, ymin = -2, xmax = -0.5, ymax = 2))
  river_network <- build_river_network(river, bbox = bbox)
  actual_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  expected_edges <- sf::st_sfc(sf::st_linestring(cbind(c(-1.5, -0.5), c(0, 0))))
  expect_setequal(expected_edges, actual_edges)
})

test_that("Endpoints are found for two intersections with network edges", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  regions <- c(sf::st_buffer(river, 2, singleSide = TRUE),
               sf::st_buffer(river, -2, singleSide = TRUE))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-1, -1))),
    sf::st_linestring(cbind(c(1, -1), c(1, 1))),
    sf::st_linestring(cbind(c(1, 1), c(1, -1))),
    sf::st_linestring(cbind(c(-1, -1), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  actual <- corridor_end_points(river_network, spatial_network, regions)
  expected <- sf::st_sfc(sf::st_point(c(0, -1)), sf::st_point(c(-1, 0)))
  expect_setequal(actual, expected)
})

test_that("Endpoints are found for more intersections with network edges", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-3, 3), c(0, 0))))
  regions <- c(sf::st_buffer(river, 2, singleSide = TRUE),
               sf::st_buffer(river, -2, singleSide = TRUE))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-1, -1))),
    sf::st_linestring(cbind(c(1, -1), c(1, 1))),
    sf::st_linestring(cbind(c(-1, -2), c(1, 1))),
    sf::st_linestring(cbind(c(1, 1), c(1, -1))),
    sf::st_linestring(cbind(c(-1, -1), c(1, -1))),
    sf::st_linestring(cbind(c(-2, -2), c(1, -1))),
    sf::st_linestring(cbind(c(-1, -2), c(-1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  actual <- corridor_end_points(river_network, spatial_network, regions)
  expected <- sf::st_sfc(sf::st_point(c(-2, 0)), sf::st_point(c(1, 0)))
  expect_setequal(actual, expected)

})

test_that("Isolated crossings are dropped when selecting end points", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-3, 3), c(0, 0))))
  regions <- c(sf::st_buffer(river, 2, singleSide = TRUE),
               sf::st_buffer(river, -2, singleSide = TRUE))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-1, -1))),
    sf::st_linestring(cbind(c(1, -1), c(1, 1))),
    sf::st_linestring(cbind(c(-1, -2), c(1, 1))),
    sf::st_linestring(cbind(c(1, 1), c(1, -1))),
    sf::st_linestring(cbind(c(-1, -1), c(1, -1))),
    sf::st_linestring(cbind(c(-2, -2), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  actual <- corridor_end_points(river_network, spatial_network, regions)
  expected <- sf::st_sfc(sf::st_point(c(-1, 0)), sf::st_point(c(1, 0)))
  expect_setequal(actual, expected)
})

test_that("An error is raised for a single intersection with network edge", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  regions <- c(sf::st_buffer(river, 2, singleSide = TRUE),
               sf::st_buffer(river, -2, singleSide = TRUE))
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-0.5, -0.5))),
    sf::st_linestring(cbind(c(0.5, 0.5), c(1, -1)))
  )
  river_network <- sfnetworks::as_sfnetwork(river, directed = FALSE)
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  expect_error(corridor_end_points(river_network, spatial_network, regions),
               "coincide")
})

test_that("River banks for a simple river gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 0), c(0, 0, -2))))
  regions <- get_river_banks(river, width = 1)
  expect_equal(length(regions), 2)
  expect_true(inherits(regions, "sfc_POLYGON"))
})

test_that("River banks for a more complex river still gives two regions", {
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 2, 2, -2),
                                              c(0.5, 0.5, -0.75, -0.75))))
  regions <- get_river_banks(river, width = 2)
  expect_equal(length(regions), 2)
  expect_true(inherits(regions, "sfc_POLYGON"))
})

test_that("River banks works with real data", {
  river <- bucharest_osm$river_centerline
  regions <- get_river_banks(river, width = 2500)
  expect_equal(length(regions), 2)
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

test_that("Capping a corridor with unknown method raises an error", {
  corridor_edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
  corridor_edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
  edges <- sf::st_sfc(corridor_edge_1, corridor_edge_2)
  expect_error(cap_corridor(edges, method = "crisp"),
               "Unknown method to cap the river corridor: crisp")
})

test_that("Capping with 'shortest-path' method raises an error if no network
          is provided", {
            corridor_edge_1 <- sf::st_linestring(cbind(c(-1, 1), c(1, 1)))
            corridor_edge_2 <- sf::st_linestring(cbind(c(-1, 1), c(-1, -1)))
            edges <- sf::st_sfc(corridor_edge_1, corridor_edge_2)
            expect_error(cap_corridor(edges, method = "shortest-path"),
                         paste("A network should be provided if",
                               "`capping_method = 'shortest-path'`"))
          })

test_that("Warning is raised if the river corridor edge did not converge", {
  p1 <- sf::st_point(c(-1, 0))
  p2 <- sf::st_point(c(-1, 1))
  p3 <- sf::st_point(c(1, 1))
  p4 <- sf::st_point(c(1, 0))
  nodes <- sf::st_sfc(p1, p4)
  edges <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(c(p1, p2, p3, p4))
  ))
  edges$from <- 1
  edges$to <- 2
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  target_edge <- sf::st_sfc(sf::st_linestring(c(p1, p4)))

  expect_warning(corridor_edge(network,
                               end_points = nodes,
                               target_edge = target_edge,
                               max_iterations = 1),
                 "River corridor edge not converged within 1 iterations")
})

#' @srrstats {G5.6, } This test verifies that the delineation is correctly
#'   identified within the provided input geometries, which has been designed
#'   with the target delineation in mind.
#' @srrstats {G5.6a} The test is considered to succeed even if a geometry that
#'   is topological equivalent to the expected geometry is recovered (so
#'   recovering the same exact target geometry is not required).
test_that("The corridor is correcly identified", {
  # The following edges form a corridor around the given river.
  # `delineate_corridor` is expected to properly identify it.
  network_edges <- sf::st_sfc(
    sf::st_linestring(cbind(c(1, -1), c(-1, -1))),
    sf::st_linestring(cbind(c(1, -1), c(1, 1))),
    sf::st_linestring(cbind(c(1, 1), c(1, -1))),
    sf::st_linestring(cbind(c(-1, -1), c(1, -1))),
    crs = 32635
  )
  spatial_network <- sfnetworks::as_sfnetwork(network_edges, directed = FALSE)
  river <- sf::st_sfc(sf::st_linestring(cbind(c(-2, 0, 2), c(0, 0, 0))),
                      crs = 32635)
  corridor_actual <- delineate_corridor(spatial_network, river,
                                        corridor_init = 1, max_width = 2)
  corridor_expected <- sf::st_polygonize(sf::st_union(network_edges)) |>
    sf::st_collection_extract()
  # Use st_equals since point order might differ
  expect_true(sf::st_equals(corridor_actual, corridor_expected, sparse = FALSE))
})
