# Directory where to save the files to test the caching mechanism of rcrisp
test_cache_dir <- testthat::test_path("testdata", "cache")
if (!dir.exists(test_cache_dir)) {
  dir.create(test_cache_dir, recursive = TRUE)
}

# Define the area of interest for OSM data and DEM
bbox <- sf::st_bbox(c(xmin = 25.95, xmax = 26.05, ymin = 44.39, ymax = 44.44),
                    crs = sf::st_crs(4326))

# Query the Overpass API and serialize the resulting osmdata_sf object as .RDS
key <- "highway"
value <- c("primary", "secondary")
osm <- osmdata_query(key, value, bbox)
saveRDS(osm, file.path(test_cache_dir, "osmdata.rds"))

# Retrieve DEM data and serialize it as .RDS
tile_urls <- get_stac_asset_urls(bbox)
dem <- load_raster(tile_urls, bbox = bbox)
# terra::SpatRaster objects must be "wrapped" before serialization
saveRDS(terra::wrap(dem), file.path(test_cache_dir, "dem.rds"))
