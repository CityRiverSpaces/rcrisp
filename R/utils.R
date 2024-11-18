#' Set the units of x as the units of y
#'
#' @param x x (can be unitless)
#' @param y y (can be unitless)
#' @return Object x with units of y
set_units_like <- function(x, y) {
  has_units_x <- inherits(x, "units")
  has_units_y <- inherits(y, "units")
  if ((!has_units_x) && (!has_units_y)) {
    return(x)
  } else if (has_units_y) {
    return(units::set_units(x, units(y), mode = "standard"))
  } else {
    return(units::drop_units(x))
  }
}

#' Get the UTM zone of a spatial object
#'
#' @param x Bounding box or geometry object
#' @return The EPSG of the UTM zone
get_utm_zone <- function(x) {
  bb <- sf::st_bbox(x)

  centroid_long <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  centroid_lat <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  base <- if (centroid_lat >= 0) 32600 else 32700
  epsg_code <- base + floor((centroid_long + 180) / 6) + 1
  return(epsg_code)
}
