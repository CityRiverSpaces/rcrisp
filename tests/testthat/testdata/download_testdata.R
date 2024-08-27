# Donwnload OSM datasets and save them for testing

# City name (in English) and river name
city <- "Bucharest"
river <- "Dâmbovița"
# Projected CRS - UTM zone 35N
epsg_code <- 32635
# OSM values for the "highway" key
highways_value <- c("motorway", "primary", "secondary", "tertiary")

.data <- NULL

get_highways <- function(bbox) {
  highways <- CRiSp::osmdata_as_sf("highway", highways_value, bbox)
  highways <- highways$osm_polygons |>
    sf::st_cast("LINESTRING") |>
    dplyr::bind_rows(highways$osm_lines) |>
    dplyr::select("highway") |>
    sf::st_transform(epsg_code)

  highways
}

get_railways <- function(bbox) {
  railways <- CRiSp::osmdata_as_sf("railway", "rail", bbox)
  highways <- railways$osm_lines |>
    sf::st_geometry() |>
    sf::st_transform(epsg_code)

  highways
}

get_city_boundary <- function(bbox, city) {
  city_boundary <- CRiSp::osmdata_as_sf("place", "city", bbox)
  city_boundary <- city_boundary$osm_multipolygons |>
    dplyr::filter(.data$`name:en` == city) |>
    sf::st_geometry() |>
    sf::st_transform(epsg_code)

  city_boundary
}

get_waterway <- function(bbox, river) {
  waterways <- CRiSp::osmdata_as_sf("waterway", "river", bbox)
  waterway <- waterways$osm_multilines |>
    dplyr::filter(.data$name == river) |>
    sf::st_geometry() |>
    sf::st_transform(epsg_code)

  waterway
}

get_waterbody <- function(bbox, waterway) {
  water <- CRiSp::osmdata_as_sf("natural", "water", bbox)
  waterbody <- dplyr::bind_rows(water$osm_polygons, water$osm_multipolygons) |>
    sf::st_transform(epsg_code) |>
    sf::st_filter(waterway, .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  waterbody
}

save_sf <- function(object, filename) {
  filepath <- testthat::test_path("testdata", filename)
  sf::st_write(object, dsn = filepath, append = FALSE)
}

download_testdata <- function() {
  bbox <- osmdata::getbb(city)

  highways <- get_highways(bbox)
  railways <- get_railways(bbox)
  city_boundary <- get_city_boundary(bbox, city)
  waterway <- get_waterway(bbox, river)
  waterbody <- get_waterbody(bbox, waterway)

  save_sf(highways, filename = sprintf("highways_%s.gpkg", city))
  save_sf(railways, filename = sprintf("railways_%s.gpkg", city))
  save_sf(city_boundary, filename = sprintf("city_boundary_%s.gpkg", city))
  save_sf(waterway, filename = sprintf("waterway_%s.gpkg", river))
  save_sf(waterbody, filename = sprintf("waterbody_%s.gpkg", river))
}

download_testdata()
