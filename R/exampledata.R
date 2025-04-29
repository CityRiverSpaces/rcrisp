get_osm_example_data <- function() {
  # Load example gpkg data from URL
  # nolint start
  url_osm <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/f519315e-b92d-4815-b924-3175bd2a7a61"
  # nolint end
  sf::read_sf(url_osm)
}

get_dem_example_data <- function() {
  # Load example DEM data from URL
  # nolint start
  url_dem <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/9eeee7aa-6005-48d6-ad63-d11c479db88b"
  # nolint end
  terra::rast(url_dem)
}
