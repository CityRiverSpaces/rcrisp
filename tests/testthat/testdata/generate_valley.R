dem <- terra::unwrap(CRiSp::bucharest_dem)
river <- CRiSp::bucharest_osm$river_surface

valley <- CRiSp::get_valley(dem, river)

filepath <- testthat::test_path("testdata", "expected_valley.gpkg")
sf::st_write(valley, filepath, append = FALSE, quiet = TRUE)
