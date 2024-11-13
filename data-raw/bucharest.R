# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

# Fetch the data
bucharest <- CRiSp::get_osmdata(city_name, river_name,
                                crs = epsg_code, buffer = bbox_buffer)

# Fix encoding issue in the WKT string of city boundary
lapply(names(bucharest), function(layer) {
  sf::st_crs(bucharest[[layer]])$wkt <- gsub("°|º", "\\\u00b0",
                                             sf::st_crs(bucharest[[layer]])$wkt)
})

# Save as package data
usethis::use_data(bucharest, overwrite = TRUE)
