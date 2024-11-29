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
#' @export
get_utm_zone <- function(x) {
  bb <- as_bbox(x)

  centroid_long <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  centroid_lat <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  base <- if (centroid_lat >= 0) 32600 else 32700
  epsg_code <- base + floor((centroid_long + 180) / 6) + 1
  return(epsg_code)
}

#' Get the bounding box from the x object. If the x does not have a CRS, WGS84
#' is assumed.
#'
#' @param x Simple feature object or a bounding box, provided either as a
#'   matrix (with x, y as rows and min, max as columns) or as a vector (xmin,
#'   ymin, xmax, ymax)
#' @return A bounding box as returned by [`sf::st_bbox()`]
#' @export
as_bbox <- function(x) {
  if (inherits(x, c("numeric", "matrix"))) {
    x <- as.vector(x)
    names(x) <- c("xmin", "ymin", "xmax", "ymax")
  }
  bbox <- sf::st_bbox(x)
  crs <- sf::st_crs(bbox)
  if (is.na(crs)) sf::st_crs(bbox) <- sf::st_crs(4326)
  return(bbox)
}
