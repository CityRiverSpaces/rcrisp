# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

# Fetch the OSM data
bucharest_osm <- CRiSp::get_osmdata(city_name, river_name,
                                    crs = epsg_code, buffer = bbox_buffer)

# Fix encoding issue in the WKT strings
fix_wkt_encoding <- function(x) {
  wkt <- sf::st_crs(x)$wkt
  sf::st_crs(x)$wkt <- gsub("°|º", "\\\u00b0", wkt)  # replace with ASCII code
  x
}
bucharest_osm <- lapply(bucharest_osm, fix_wkt_encoding)

# Fetch the DEM data
bucharest_dem <- CRiSp::get_dem(bucharest_osm$bb) |>
  CRiSp::reproject(epsg_code) |>
  # SpatRaster objects cannot be directly serialized as RDS/RDA files
  terra::wrap()

# Save as package data
usethis::use_data(bucharest_osm, overwrite = TRUE)
usethis::use_data(bucharest_dem, overwrite = TRUE)
