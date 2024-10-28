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

  net
}

#' Flatten a network by adding points at apparent intersections
#'
#' This function adds nodes at intersections between network edges that
#' cross each other but do not have vertices at the intersection.
#'
#' @param network A network object
#'
#' @return A network object with additional points at intersections
#' @export
flatten_network <- function(network) {
  # Determine intersection points between crossing edges
  edges_cross <- get_crossing_edges(network)
  pts_intersect <- get_intersection_points(edges_cross)

  # Convert edge table to data.frame and add info on boundary points
  edge_pts <- sfheaders::sf_to_df(edges_cross)
  edge_idxs <- edge_pts$linestring_id
  edge_pts$is_startpoint <- !duplicated(edge_idxs)
  edge_pts$is_endpoint <- !duplicated(edge_idxs, fromLast = TRUE)

  # Loop over all points, add them to the edge table
  for (i in seq_len(nrow(pts_intersect))) {
    point <- pts_intersect$geometry[[i]]
    intersecting_edges <- unique(unlist(pts_intersect$origins[i]))
    for (edge_id in intersecting_edges){
      edge_pts <- insert_intersection(edge_pts, point, edge_id)
    }
  }

  # Convert back edge table to sfc object
  edges_cross_new <- sfheaders::sfc_linestring(edge_pts, linestring_id = "id",
                                               x = "x", y = "y")
  sf::st_crs(edges_cross_new) <- sf::st_crs(edges_cross)

  # Update the network with the new edge geometries
  nodes <- network |> sf::st_as_sf("nodes")
  edges <- network |> sf::st_as_sf("edges")
  edges[edges_cross$id, ] <- edges[edges_cross$id, ] |>
    sf::st_set_geometry(edges_cross_new)
  network_new <- sfnetworks::sfnetwork(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    force = TRUE,  # skip checks
  )
  network_new
}

get_crossing_edges <- function(network) {
  network |>
    tidygraph::activate("edges") |>
    # Add ID to ease replacement later on
    dplyr::mutate(id = seq_len(dplyr::n())) |>
    dplyr::filter(sfnetworks::edge_crosses(tidygraph::.E())) |>
    sf::st_as_sf("edges")
}

get_intersection_points <- function(edges) {
  pts_intersect <- edges |>
    sf::st_intersection() |>
    # Cast multipoint intersections into points
    sf::st_collection_extract("POINT") |>
    sfheaders::sf_cast(to = "POINT")

  pts_intersect_agg <- aggregate(
    pts_intersect,
    by = sf::st_geometry(pts_intersect),
    FUN = unique,
    drop = TRUE
  )

  pts_intersect_unique <- pts_intersect_agg |> dplyr::distinct()
  pts_intersect_unique
}

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

insert_intersection <- function(edge_pts, point, line_id) {
  line_pts <- subset(edge_pts, linestring_id == line_id)
  pt_x <- point[[1]]
  pt_y <- point[[2]]
  is_point_in_line <- nrow(subset(line_pts, x == pt_x &
                                    y == pt_y)) >= 1
  if (!is_point_in_line) {
    startpoint <- subset(line_pts, is_startpoint == TRUE)
    kk <- as.numeric(rownames(startpoint))
    w_break <- FALSE
    while (!w_break) {
      # Consider the line segments a - b.
      # x is a valid intersection if the following condition is true:
      # distance(a, b) == distance(a, x) + distance(x, b)
      pt_a_x <- edge_pts[kk, ]$x
      pt_a_y <- edge_pts[kk, ]$y
      pt_b_x <- edge_pts[kk + 1, ]$x
      pt_b_y <- edge_pts[kk + 1, ]$y
      d_ab <- distance(pt_a_x, pt_a_y, pt_b_x, pt_b_y)
      d_ax <- distance(pt_a_x, pt_a_y, pt_x, pt_y)
      d_bx <- distance(pt_b_x, pt_b_y, pt_x, pt_y)
      is_intersection <- dplyr::near(d_ab, d_ax + d_bx, tol = 1.e-3)
      if (is_intersection) {
        insertion <- tibble::tibble_row(
          sfg_id = line_id,
          linestring_id = line_id,
          x = pt_x,
          y = pt_y,
          is_startpoint = FALSE,
          is_endpoint = FALSE
        )
        edge_pts <- tibble::add_row(edge_pts, insertion, .after = kk)
        w_break <- TRUE
      } else {
        if (edge_pts[kk + 1, ]$is_endpoint) {
          warning("point is not added to the edge df.")
          w_break <- TRUE
        }
      }
      kk <- kk + 1
    }
  }
  edge_pts
}
