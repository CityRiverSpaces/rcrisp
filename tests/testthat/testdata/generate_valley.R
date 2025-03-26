dem <- terra::unwrap(CRiSpData::bucharest_dem)
river <- CRiSpData::bucharest_osm$river_surface

valley <- CRiSp::delineate_valley(dem, river)

filepath <- testthat::test_path("testdata", "expected_valley.gpkg")
sf::st_write(valley, filepath, append = FALSE, quiet = TRUE)
