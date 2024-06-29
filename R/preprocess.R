# TODO if we can avoid adding lines from polygons, we should remove this
#' Merge streets from polygons and lines
#'
#' @param highways A list object containing streets from OpenStreetMap
#'
#' @return A data frame with a column named 'highway' containing line strings
#' @export
merge_streets <- function(highways) {
  poly_to_lines <- highways$osm_polygons |>
    sf::st_cast("LINESTRING")
  highways_lines <- highways$osm_lines |>
    dplyr::bind_rows(poly_to_lines)
  highways_lines
}

#' Create a network from a line strings
#'
#' @param data A data frame with a column named 'highway' containing
#'             line strings
#' @param crs A coordinate reference system as an epsg code, e.g. 4326 for WGS84
#'
#' @return A network object
#' @export
create_network <- function(data, crs = NULL) {
  net <- data |>
    sfnetworks::as_sfnetwork(directed = FALSE)

  if (!is.null(crs)) {
    net <- net |> sf::st_transform(crs)
  }
}
