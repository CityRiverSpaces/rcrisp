bucharest_dem <- get_dem_example_data()
bucharest_osm <- get_osm_example_data()

dem <- bucharest_dem
river <- bucharest_osm$river_surface

valley <- CRiSp::delineate_valley(dem, river)

filepath <- testthat::test_path("testdata", "expected_valley.gpkg")
sf::st_write(valley, filepath, append = FALSE, quiet = TRUE)
