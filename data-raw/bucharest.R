# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

# Fix encoding issue for linter in the WKT string
crs_wkt <- sf::st_crs(epsg_code)$wkt
crs_wkt_fixed <- gsub("°|º", "\\\u00b0", crs_wkt)

# Fetch the OSM data
bucharest <- CRiSp::get_osmdata(city_name, river_name,
                                crs = crs_wkt_fixed, buffer = bbox_buffer)
# Add the DEM
dem <- CRiSp::get_dem(bucharest$bb)
bucharest <- c(bucharest, dem = reproject(dem, crs_wkt_fixed))

# Save as package data
usethis::use_data(bucharest, overwrite = TRUE)
