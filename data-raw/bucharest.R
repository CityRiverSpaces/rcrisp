# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
river_buffer <- 2000

# Fetch the OSM data
bucharest_osm <- get_osmdata(
  city_name,
  river_name,
  crs = epsg_code,
  buffer_distance = river_buffer,
  force_download = TRUE
)

# Fix encoding issue in the WKT strings
fix_wkt_encoding <- function(x) {
  wkt <- sf::st_crs(x)$wkt
  sf::st_crs(x)$wkt <- gsub("°|º", "\\\u00b0", wkt)  # replace with ASCII code
  x
}
bucharest_osm <- lapply(bucharest_osm, fix_wkt_encoding)

# Fetch the DEM data
bbox <- as_bbox(bucharest_osm$aoi)
crs <- get_utm_zone(bbox)
bucharest_dem <- get_dem(bbox, crs = crs, force_download = TRUE)

# Save as package data
usethis::use_data(bucharest_osm, overwrite = TRUE)
usethis::use_data(bucharest_dem, overwrite = TRUE)
