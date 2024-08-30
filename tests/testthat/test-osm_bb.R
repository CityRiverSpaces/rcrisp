test_that("OSM bounding box is correctly retreived from Overpass API", {
  skip_on_ci()

  bb <- CRiSp::osm_bb("Bucharest")
  expected <- matrix(c(25.966674, 44.334247, 26.225577, 44.541396),
                     ncol = 2, dimnames = list(c("x", "y"), c("min", "max")))
  expect_equal(bb, expected)
})
