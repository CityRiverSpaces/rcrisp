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