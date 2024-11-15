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

#' Get UTM zone from sf object
#'
#' @param x An sf object
#'
#' @return The UTM zone
#' @export
get_utm_zone_epsg_sf <- function(x) {
  if (!"sf" %in% class(x)) {
    stop("x must be an sf object")
  }

  coords <- x |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_centroid() |>
    sf::st_transform(4326) |>
    sf::st_coordinates()

  if (coords[2] >= 0L) {
    base <- 32600
  } else {
    base <- 32700
  }
  base + floor((coords[1] + 180) / 6) + 1
}

#' Get UTM zone from bbox object
#'
#' @param bb A bbox object
#'
#' @return The UTM zone
#' @export
get_utm_zone_epsg_bbox <- function(bb) {
  if (!inherits(bb, "bbox")) {
    stop("the bounding box must be of class bbox")
  }

  centroid_long <- (bb[1] + bb[3]) / 2
  centroid_lat <- (bb[2] + bb[4]) / 2

  base <- if (centroid_lat >= 0) 32600 else 32700
  utm_zone <- base + floor((centroid_long + 180) / 6) + 1

  return(utm_zone)
}
