library(CRiSp)
library(osmdata)
library(dplyr)
library(sf)

# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000

# Fetch the data
bucharest <- get_osmdata(city_name, river_name,
                         crs = epsg_code, buffer = bbox_buffer)

# Fix encoding issue in the WKT string
st_crs(bucharest$boundary)$wkt <- gsub("°|º", "\\\u00b0",
                                  st_crs(bucharest$boundary)$wkt)

# Save as package data
usethis::use_data(bucharest, overwrite = TRUE)
