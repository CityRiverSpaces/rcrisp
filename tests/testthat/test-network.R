#          p4
#          |
# p1 - p2 ---- p3
#          |
#         p5
p1 <- sf::st_point(c(0, 0))
p2 <- sf::st_point(c(1, 0))
p3 <- sf::st_point(c(3, 0))
p4 <- sf::st_point(c(2, 1))
p5 <- sf::st_point(c(2, -1))

e1 <- sf::st_linestring(c(p1, p2))
e2 <- sf::st_linestring(c(p2, p3))
e3 <- sf::st_linestring(c(p4, p5))

test_that("Network objects can be set up with no modifications", {
  edges <- sf::st_sfc(e1, e2, e3)
  network <- as_network(edges, flatten = FALSE, clean = FALSE)
  expect_true(inherits(network, "sfnetwork"))
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network, "nodes"))
  edges_actual <- sf::st_as_sf(network, "edges")
  nodes_expected <- sf::st_sfc(p1, p2, p3, p4, p5)
  from_expected <- c(1, 2, 4)
  to_expected <- c(2, 3, 5)
  expect_setequal(sf::st_geometry(edges_actual), edges)
  expect_setequal(edges_actual$from, from_expected)
  expect_setequal(edges_actual$to, to_expected)
  expect_setequal(nodes_actual, nodes_expected)
})

test_that("Network flattening inject intersection within edges", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  edges <- sf::st_as_sf(sf::st_sfc(e2, e3))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_flat <- flatten_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_flat, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_flat, "edges"))
  intersection <- sf::st_intersection(e2, e3)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p2, intersection, p3)),
                               sf::st_linestring(c(p4, intersection, p5)))
  expect_setequal(nodes_actual, nodes)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning transforms shared internal points to nodes", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  intersection <- sf::st_intersection(e2, e3)
  edges <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(c(p2, intersection, p3)),
                                   sf::st_linestring(c(p4, intersection, p5))))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p2, p3, p4, p5, intersection)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p2, intersection)),
                               sf::st_linestring(c(intersection, p3)),
                               sf::st_linestring(c(p4, intersection)),
                               sf::st_linestring(c(intersection, p5)))
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning drops pseudo nodes", {
  nodes <- sf::st_sfc(p1, p2, p3)
  edges <- sf::st_as_sf(sf::st_sfc(e1, e2))
  edges$from <- c(1, 2)
  edges$to <- c(2, 3)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p1, p3)
  edges_expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)))
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network cleaning drops disconnected components", {
  nodes <- sf::st_sfc(p2, p3, p4, p5)
  edges <- sf::st_as_sf(sf::st_sfc(e2, e3))
  edges$from <- c(1, 3)
  edges$to <- c(2, 4)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_clean <- clean_network(network)
  nodes_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_actual <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  nodes_expected <- sf::st_sfc(p2, p3)
  edges_expected <- sf::st_sfc(e2)
  expect_setequal(nodes_actual, nodes_expected)
  expect_setequal(edges_actual, edges_expected)
})

test_that("Network simplification drops loops and multiple edges", {
  nodes <- sf::st_sfc(p2, p3)
  p6 <- sf::st_point(c(3, -1))
  edges <- sf::st_as_sf(sf::st_sfc(e2,
                                   sf::st_linestring(c(p2, p4, p3)),
                                   sf::st_linestring(c(p3, p5, p6, p3))))
  edges$from <- c(1, 1, 2)
  edges$to <- c(2, 2, 2)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_simplified <- simplify_network(network)
  nodes_simplified <- sf::st_geometry(sf::st_as_sf(network_simplified, "nodes"))
  edges_simplified <- sf::st_geometry(sf::st_as_sf(network_simplified, "edges"))
  edges_expected <- sf::st_sfc(e2)
  expect_setequal(nodes_simplified, nodes)
  expect_setequal(edges_simplified, edges_expected)
  # Also check that the simplification is run as part of the network cleaning
  network_clean <- clean_network(network)
  nodes_clean <- sf::st_geometry(sf::st_as_sf(network_clean, "nodes"))
  edges_clean <- sf::st_geometry(sf::st_as_sf(network_clean, "edges"))
  expect_setequal(nodes_clean, nodes_simplified)
  expect_setequal(edges_clean, edges_simplified)
})

test_that("Network setup with real data", {
  edges <- bucharest$streets
  network <- as_network(edges, clean = FALSE, flatten = FALSE)
  edges_actual <- sf::st_geometry(sf::st_as_sf(network, "edges"))
  edges_expected <- sf::st_geometry(edges)
  expect_setequal(edges_actual, edges_expected)
})