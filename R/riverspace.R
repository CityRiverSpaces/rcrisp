#' Get viewpoints along a line
#'
#' @param x object of class sf, sfc or sfg
#' @param density number of points per distance unit
#'
#' @return object of class sfc_POINT
#' @export
get_viewpoints <- function(x, density = 1/50) {
  sf::st_line_sample(x, density = density) |> sf::st_cast("POINT")
}

#' Get rays from a viewpoint within a maximum isovist
#'
#' @param maxisovist object of class sf, sfc or sfg
#' @param vpoint object of class sf, sfc or sfg
#'
#' @return object of class sfc_LINESTRING
#' @export
get_rays <- function(maxisovist, vpoint) {
  rayvertices <- sf::st_cast(maxisovist,"POINT")
  rays <- lapply(X = seq_along(rayvertices), FUN = \(x) {
    pair <- sf::st_combine(c(rayvertices[x], vpoint))
    line <- sf::st_cast(pair, "LINESTRING")
  })
  rays <- do.call(c, rays)
  rays <- st_sf(geometry = rays,
                id = seq_along(rays))
  rays
}

#' Cast rays to points
#'
#' @param rays object of class sf, sfc or sfg
#' @param geom_type 'LINESTRING' or 'MULTILINESTRING'
#'
#' @return object of class sfc_POINT
#' @export
process_rays <- function(rays, geom_type) {
  rays |>
    dplyr::filter(sf::st_is(geometry, geom_type)) |>
    sf::st_cast("POINT")
}

#' Get furthest vertex from a set of points
#'
#' @param points object of class sf, sfc or sfg
#' @param id_col name of the id column
#'
#' @return object of class sfc_POINT
#' @export
get_furthest_vertex <- function(points, id_col = "id") {
  points |>
    dplyr::group_by({{ id_col }}) |>
    dplyr::slice_tail(n = 2) |>
    dplyr::slice_head(n = 1) |>
    dplyr::summarise(do_union = FALSE, .groups = 'drop') |>
    sf::st_cast("POINT")
}

#' Calculate isovist from a viewpoint
#'
#' @param occluders object of class sf, sfc or sfg
#' @param vpoint object of class sf, sfc or sfg
#' @param rayno number of rays
#' @param raylen length of rays
#'
#' @return object of class sfc_POLYGON
#' @export
get_isovist <- function(occluders, vpoint, rayno = 41, raylen = 100) {
  maxisovist <- sf::st_buffer(vpoint, dist = raylen, nQuadSegs = (rayno-1)/4)
  occ_intersections <- sf::st_intersects(occluders, maxisovist, sparse = FALSE)

  if (!any(occ_intersections)){
    isovist <- maxisovist
  } else {
    rays <- get_rays(maxisovist, vpoint)

    occ_in_maxisovist <- occluders[occ_intersections] |> st_union()
    rays_outside_occ <- sf::st_difference(rays, occ_in_maxisovist)

    # Get furthest vertex of ray segment closest to view point
    nonoccluded_end <- process_rays(rays_outside_occ, "LINESTRING") |>
      get_furthest_vertex()
    occluded_end  <- process_rays(rays_outside_occ, "MULTILINESTRING") |>
      get_furthest_vertex()

    # Combine vertices, order by ray angle and casting to polygon
    all_vertices <- rbind(nonoccluded_end, occluded_end) |> dplyr::arrange(id)
    isovist  <- sf::st_combine(all_vertices) |> sf::st_cast("POLYGON")
  }
  isovist
}
