test_that("The street network is properly setup from the highway geometries", {
  highways_path <- testthat::test_path("testdata", "highways_Bucharest.gpkg")
  highways <- sf::st_read(highways_path, quiet = TRUE)
  net <- CRiSp::create_network(highways)

  # class is  correct
  expect_true(sfnetworks::is.sfnetwork(net))

  # edges are properly set
  edges <- sfnetworks::activate(net, "edges") |> sf::st_geometry()
  edges_expected <- sf::st_geometry(highways)
  expect_equal(edges, edges_expected)

  # nodes are properly identified (size gt 0)
  nodes <- sfnetworks::activate(net, "nodes") |> sf::st_geometry()
  expect_gt(length(nodes), 0)
})

test_that("The CRS of the network can be optionally modified", {
  highways_path <- testthat::test_path("testdata", "highways_Bucharest.gpkg")
  highways <- sf::st_read(highways_path, quiet = TRUE)
  epsg_code <- 4326
  net <- CRiSp::create_network(highways, crs = epsg_code)  # set to WGS84

  crs <- sf::st_crs(net)
  expect_equal(crs$epsg, epsg_code)
})

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

e1 <- sf::st_cast(sf::st_union(p1, p2), "LINESTRING")
e2 <- sf::st_cast(sf::st_union(p2, p3), "LINESTRING")
e3 <- sf::st_cast(sf::st_union(p4, p5), "LINESTRING")

test_that("Network flattening results in correct number of segments", {
  nodes <- sf::st_as_sf(sf::st_sfc(p1, p2, p3, p4, p5))
  edges <- sf::st_as_sf(sf::st_sfc(e1, e2, e3))
  edges$from <- c(1, 2, 4)
  edges$to <- c(2, 3, 5)
  network <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                                   directed = FALSE, force = TRUE,
                                   node_key = "x")
  network_new <- flatten_network(network)
  network_clean <- clean_network(network_new)
  nsegs_actual <- network_clean |>
    tidygraph::activate("edges") |>
    sf::st_as_sf() |>
    lwgeom::st_split(network_clean |> tidygraph::activate("nodes") |> sf::st_as_sf()) |>
    nrow()
  expect_equal(nsegs_actual, 4)
})
