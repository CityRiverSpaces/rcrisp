## code to prepare `bucharest` dataset goes here

city_name <- "Bucharest"

bucharest_boundary <- get_osm_city_boundary(city_name)

bucharest <- list(
  boundary = bucharest_boundary
)

usethis::use_data(bucharest, overwrite = TRUE)
