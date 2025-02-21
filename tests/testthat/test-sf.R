test_that("Splitting a geometry by a complex line returns more regions", {
  line <- sf::st_sfc(c(sf::st_linestring(cbind(c(-2, 2), c(0, 0))),
                       sf::st_linestring(cbind(c(-0.5, 0, 0.5), c(0, 0.5, 0)))))
  geometry <- sf::st_sfc(sf::st_polygon(list(cbind(c(-1, -1, 1, 1, -1),
                                                   c(-1, 1, 1, -1, -1)))))
  regions <- split_by(geometry, line)
  expect_equal(length(regions), 3)
})

test_that("Splitting a geometry by a complex line still returns two edges", {
  line <- sf::st_sfc(c(sf::st_linestring(cbind(c(-2, 2), c(0, 0))),
                       sf::st_linestring(cbind(c(-0.5, 0, 0.5), c(0, 0.5, 0)))))
  geometry <- sf::st_sfc(sf::st_polygon(list(cbind(c(-1, -1, 1, 1, -1),
                                                   c(-1, 1, 1, -1, -1)))))
  edges <- split_by(geometry, line, boundary = TRUE)
  expect_equal(length(edges), 2)
})

test_that("Holes are removed from a geometry", {
  geometry <-
    sf::st_sfc(sf::st_polygon(list(cbind(c(-1, -1, 1, 1, -1),
                                         c(-1, 1, 1, -1, -1)),
                                   cbind(c(-0.5, -0.5, 0.5, 0.5, -0.5),
                                         c(-0.5, 0.5, 0.5, -0.5, -0.5)))))
  new_geometry <- remove_holes(st_multipolygon(geometry))
  expect_equal(length(new_geometry), 1)
})
