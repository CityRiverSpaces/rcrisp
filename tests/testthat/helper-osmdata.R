#' Gererate artificial OSM-like data
#'
#' This helper function returns a list with the same entries as the one
#' returned by [`get_osmdata()`]. This function can thus be used to mock
#' the call to the main rcrisp function, without the need to interact with the
#' overpass API or other remote services.
get_test_osmdata <- function() {
  # Pick a projected CRS (UTM Zone 1N)
  crs <- "EPSG:32601"
  # Define the city boundary and, from it, derive the bbox
  boundary <- sf::st_sfc(
    sf::st_boundary(sf::st_buffer(sf::st_point(c(0, 0)), 7000)),
    crs = crs
  )
  # Bounding box should be in lat/lon
  bb <- sf::st_bbox(boundary) |> sf::st_transform("EPSG:4326")
  # Draw the river centerline
  river_centerline <- sf::st_sfc(
    sf::st_linestring(cbind(c(-10000, 0, 10000), c(1000, 0, -1000))),
    crs = crs
  )
  # Draw the buffer regions for streets/railways and buildings, both in lat/lon
  aoi_network <- sf::st_buffer(river_centerline, 3000) |>
    sf::st_transform(sf::st_crs("EPSG:4326"))
  aoi_buildings <- sf::st_buffer(river_centerline, 100) |>
    sf::st_transform(sf::st_crs("EPSG:4326"))
  # Draw some water surfaces (they should all intersect the river centerline)
  river_surface <- sf::st_sfc(
    sf::st_multipolygon(list(
      sf::st_buffer(sf::st_point(c(-2500, 250)), 100),
      sf::st_buffer(sf::st_point(c(0, 0)), 100),
      sf::st_buffer(sf::st_point(c(2500, -250)), 100)
    )),
    crs = crs
  )
  # Draw the street network, with some river crossings
  streets <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(cbind(c(-10000, 10000), c(2000, 0))),
    sf::st_linestring(cbind(c(-5000, -5000), c(-10000, 10000))),
    sf::st_linestring(cbind(c(-3000, -3000), c(-10000, 10000))),
    sf::st_linestring(cbind(c(0, 0), c(-10000, 10000))),
    sf::st_linestring(cbind(c(3000, 3000), c(-10000, 10000))),
    sf::st_linestring(cbind(c(5000, 5000), c(-10000, 10000))),
    sf::st_linestring(cbind(c(-10000, 10000), c(0, -2000))),
    sf::st_boundary(sf::st_buffer(sf::st_point(c(0, 0)), 6000)),
    crs = crs
  ))
  # Draw the railway network, with some river crossings
  railways <- sf::st_as_sf(sf::st_sfc(
    sf::st_linestring(cbind(c(-10000, 10000), c(2500, 500))),
    sf::st_linestring(cbind(c(-2500, -2500), c(-10000, 10000))),
    sf::st_linestring(cbind(c(2500, 2500), c(-10000, 10000))),
    sf::st_linestring(cbind(c(-10000, 10000), c(-500, -2500))),
    sf::st_boundary(sf::st_buffer(sf::st_point(c(0, 0)), 3000)),
    crs = crs
  ))
  # Draw some buildings in the proximity of the river centerline/surface
  buildings <- sf::st_sfc(
    sf::st_multipolygon(list(
      sf::st_polygon(list(cbind(
        c(-2550, -2550, -2450, -2450, -2550),
        c(450, 400, 400, 450, 450)
      ))),
      sf::st_polygon(list(cbind(
        c(-50, -50, 50, 50, -50),
        c(200, 150, 150, 200, 200)
      ))),
      sf::st_polygon(list(cbind(
        c(-50, -50, 50, 50, -50),
        c(-200, -150, -150, -200, -200)
      ))),
      sf::st_polygon(list(cbind(
        c(2550, 2550, 2450, 2450, 2550),
        c(-450, -400, -400, -450, -450)
      )))
    )),
    crs = crs
  )
  # Return the list with all components
  list(
    bb = bb,
    boundary = boundary,
    river_centerline = river_centerline,
    river_surface = river_surface,
    streets = streets,
    railways = railways,
    buildings = buildings,
    aoi_network = aoi_network,
    aoi_buildings = aoi_buildings
  )
}
