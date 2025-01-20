#' Get riverspace surrounding a river
#'
#' @param occluders Geometry of occluders
#' @param river River centreline or surface
#' @param rayno Number of rays
#' @param raylen Length of rays
#'
#' @return Polygon geometry with the riverspace
#' @export
#'
#' @examples
#' get_riverspace(bucharest_osm$buildings, bucharest_osm$river_surface)
get_riverspace <- function(occluders, river, density = 1/50,
                           rayno = 41, raylen = 100) {
  vpoints <- visor::get_viewpoints(river, density = density)
  isovists <- lapply(vpoints, \(vpoint) {
    visor::get_isovist(occluders, vpoint, rayno, raylen)
  })
  sf::st_union(isovists)
}
