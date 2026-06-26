#' Delineate the space surrounding a river
#'
#' @param river List with river surface and centerline
#' @param occluders Geometry of occluders
#' @param density Density of viewpoints
#' @param ray_num Number of rays as numeric vector of length one
#' @param ray_length Length of rays in meters as numeric vector of length one
#'
#' @details For riverspace delineation, OSM water polygons intersecting the
#'   river centerline are treated as part of the river, including connected
#'   water bodies such as lakes and reservoirs. Viewpoints are calculated from
#'   water edges where available, and from the river centerline when river
#'   surface polygons are missing or partial.
#'
#' @return Riverspace as object of class [`sf::sfc_POLYGON`]
#' @export
#'
#' @examplesIf interactive()
#' bucharest_osm <- get_osm_example_data()
#' delineate_riverspace(bucharest_osm$river_surface, bucharest_osm$buildings)
#' @srrstats {G2.6} One-dimensional distance input is pre-processed by
#'   `preprocess_distance()` to handle `units` objects or other vector-like
#'   classes with storage mode `numeric`.
#' @srrstats {G2.7} The `river` and `occluders` parameters accept
#'   domain-specific tabular input of type `sf`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sf::sfc_POLYGON`], explicitly documented as such, and it maintains the
#'   same units as the input.
delineate_riverspace <- function(river, occluders = NULL, density = 1 / 50,
                                 ray_num = 40, ray_length = 100) {
  # Pre-process distance input
  ray_length <- preprocess_distance(ray_length)
  # Check input
  checkmate::assert_multi_class(river, c("list", "sf", "sfc"))
  checkmate::assert_vector(river, min.len = 1)
  checkmate::assert_multi_class(occluders, c("sf", "sfc"), null.ok = TRUE)
  checkmate::assert_numeric(density, len = 1)
  checkmate::assert_numeric(ray_num, len = 1)
  checkmate::assert_numeric(ray_length, len = 1)
  checkmate::assert_true(as_crs(river) == as_crs(occluders))

  viewpoints <- visor::get_viewpoints(river, density = density)
  visor::get_isovist(viewpoints, occluders = occluders, ray_num = ray_num,
                     ray_length = ray_length)
}
