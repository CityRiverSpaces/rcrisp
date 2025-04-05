# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
network_buffer <- 2500
buildings_buffer <- 100
dem_buffer <- 2500

# Add delineation to package data
bucharest_delineation <- delineate(city_name, river_name, crs = epsg_code,
                                   network_buffer = network_buffer,
                                   buildings_buffer = buildings_buffer,
                                   dem_buffer = dem_buffer,
                                   corridor = TRUE, segments = TRUE,
                                   riverspace = TRUE)

# Fix encoding issue in the WKT strings
fix_wkt_encoding <- function(x) {
  wkt <- sf::st_crs(x)$wkt
  sf::st_crs(x)$wkt <- gsub("°|º", "\\\u00b0", wkt)  # replace with ASCII code
  x
}
bucharest_delineation <- lapply(bucharest_delineation, fix_wkt_encoding)

# Save as package data
usethis::use_data(bucharest_delineation, overwrite = TRUE, compress = "xz")
