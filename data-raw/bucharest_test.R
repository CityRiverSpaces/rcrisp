dem <- terra::unwrap(bucharest_dem)
river <- bucharest_osm$river_surface
crs <- "epsg:32635"

valley <- get_valley(dem, river, crs)

if (!dir.exists("tests/testthat/fixtures")) {
  dir.create("tests/testthat/fixtures", recursive = TRUE)
}

sf::st_write(valley, "tests/testthat/fixtures/expected_valley.gpkg",
             overwrite = TRUE)
