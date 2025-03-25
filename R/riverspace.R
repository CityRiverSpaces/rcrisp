#' Delineate the space surrounding a river
#'
#' @param river List with river surface and centerline
#' @param occluders Geometry of occluders
#' @param density Density of viewpoints
#' @param ray_num Number of rays
#' @param ray_length Length of rays in meters
#'
#' @return Polygon geometry with the riverspace
#' @export
#'
#' @examples
#' \dontrun{
#'   delineate_riverspace(bucharest_osm$river_surface, bucharest_osm$buildings)
#' }
delineate_riverspace <- function(river, occluders = NULL, density = 1 / 50,
                                 ray_num = 41, ray_length = 100) {
  viewpoints <- visor::get_viewpoints(river, density = density)
  visor::get_isovist(viewpoints, occluders = occluders, ray_num = ray_num,
                     ray_length = ray_length)
}
