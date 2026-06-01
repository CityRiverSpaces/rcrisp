#' Convert points to linestring
#'
#' @noRd
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class
#'   [`sf::sfc_LINESTRING`] and it maintains the same units as the input.
as_linestring <- function(points) {
  points_union <- sf::st_union(points)
  sf::st_cast(points_union, "LINESTRING")
}

#' Convert linestring to polygon
#'
#' @noRd
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class
#'   [`sf::sfc_POLYGON`] and it maintains the same units as the input.
as_polygon <- function(lines) {
  lines_union <- sf::st_union(lines)
  sf::st_line_merge(lines_union) |>
    sf::st_polygonize() |>
    sf::st_collection_extract()
}

#' Convert a geometry to a simple feature collection
#'
#' @noRd
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class
#'   [`sf::sfc`] and it maintains the same units as the input.
as_sfc <- function(x) {
  if (inherits(x, "sfc")) {
    x
  } else {
    sf::st_as_sfc(x)
  }
}

#' Get the index of the n geometries with largest area
#'
#' @noRd
find_largest <- function(geometry, n = 1) {
  area <- sf::st_area(geometry)
  order(area, decreasing = TRUE)[seq_len(n)]
}

#' Get the index of the n geometries with highest length
#'
#' @noRd
find_longest <- function(geometry, n = 1) {
  length <- sf::st_length(geometry)
  order(length, decreasing = TRUE)[seq_len(n)]
}

#' Split a geometry along a (multi)linestring.
#'
#' @param geometry Geometry to split
#' @param line Dividing (multi)linestring
#' @param boundary Whether to return the split boundary instead of the regions
#'
#' @return An object of class [`sf::sfc`]
#' @keywords internal
#' @srrstats {SP4.0, SP4.0a, SP4.1} The return value is of class
#'   [`sf::sfc`], with the same type and units as the input geometry.
split_by <- function(geometry, line, boundary = FALSE) {
  regions <- lwgeom::st_split(geometry, line) |>
    sf::st_collection_extract()
  if (!boundary) {
    regions
  } else {
    boundaries <- sf::st_boundary(regions)
    sf::st_difference(boundaries, line)
  }
}
