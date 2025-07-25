#' Create a network from a collection of line strings.
#'
#' @param edges An [`sf::sf`] or [`sf::sfc_LINESTRING`] object with the
#'   network edges
#' @param flatten Whether all intersections between edges should be
#'   converted to nodes
#' @param clean Whether general cleaning tasks should be run on the generated
#'   network (see [`clean_network()`] for the description of tasks)
#'
#' @return An [`sfnetworks::sfnetwork`] object
#' @export
#' @examples
#' edges <- sf::st_sfc(
#'   sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
#'   sf::st_linestring(matrix(c(0, 1, 1, 0), ncol = 2, byrow = TRUE)),
#'   crs = sf::st_crs("EPSG:4326")
#' )
#'
#' # Run with default values
#' as_network(edges)
#'
#' # Only build the spatial network
#' as_network(edges, flatten = FALSE, clean = FALSE)
#' @srrstats {G2.7} The `edges` parameter only accepts tabular input of class
#'   `sf`. `sfnetwork` objects are `sf`-compatible and are commonly used
#'   for spatial network analysis.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], explicitly documented as such, and it maintains
#'   the same units as the input.
as_network <- function(edges, flatten = TRUE, clean = TRUE) {
  # Check input
  checkmate::assert_true(inherits(edges, c("sf", "sfc")))
  checkmate::assert_logical(flatten, len = 1)
  checkmate::assert_logical(clean, len = 1)

  network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
  if (flatten) network <- flatten_network(network)
  if (clean) network <- clean_network(network)
  network
}

#' Flatten a network by adding points at apparent intersections.
#'
#' All crossing edges are identified, and the points of intersections are
#' injected within the edge geometries. Note that the injected points are
#' not converted to network nodes (this can be achieved via sfnetworks'
#' [`sfnetworks::to_spatial_subdivision()`], which is part of the tasks
#' that are included in [`clean_network()`].
#'
#' The functionality is similar to sfnetworks'
#' [`sfnetworks::st_network_blend()`], but in that case an external point is
#' only injected to the closest edge.
#'
#' @param network A network object of class [`sfnetworks::sfnetwork`]
#'
#' @return An [`sfnetworks::sfnetwork`] object with additional points at
#'   intersections
#' @export
#' @examples
#' bucharest_osm <- get_osm_example_data()
#' edges <- dplyr::bind_rows(bucharest_osm$streets,
#'                           bucharest_osm$railways)
#' network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
#' flatten_network(network)
#' @srrstats {G2.7} The `network` object provided as input must be of class
#'   `sfnetwork`. `sfnetwork` objects are `sf`-compatible and are commonly used
#'   for spatial network analysis.
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], same as the input class, explicitly documented
#'   as such, and it maintains the same units as the input.
flatten_network <- function(network) {
  nodes <- sf::st_as_sf(network, "nodes")
  edges <- sf::st_as_sf(network, "edges")

  # Determine intersection points between crossing edges
  edges_cross <- get_crossing_edges(edges)
  if (nrow(edges_cross) == 0) return(network)
  points_intersect <- get_intersection_points(edges_cross)

  # Add target points to the edge geometries
  edges_new <- insert_intersections(edges_cross, points_intersect)

  # Update the network with the new edge geometries
  edges[edges_cross$id, ] <- sf::st_set_geometry(edges[edges_cross$id, ],
                                                 edges_new)
  network_new <- sfnetworks::sfnetwork(nodes = nodes,
                                       edges = edges,
                                       directed = FALSE,
                                       force = TRUE)  # skip checks
  network_new
}

#' Get edges that cross each other.
#'
#' @noRd
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `edges`. This is used when only
#'   geometry information is needed from that point onwards and all other
#'   attributes (i.e., columns) can be safely discarded. The object returned
#'   by `sf::st_geometry()` is a simple feature geometry list column of class
#'   `sfc`.
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class [`sf::sf`] and
#'   it maintains the same units as the input.
get_crossing_edges <- function(edges) {
  geometry <- sf::st_geometry(edges)
  crossings <- sf::st_crosses(geometry) |> suppressMessages()
  mask <- lengths(crossings) > 0
  sf::st_sf(id = which(mask), geometry = geometry[mask])
}

#' Get intersection points between edges.
#'
#' @noRd
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class
#'   [`sf::sfc_POINT`] and it maintains the same units as the input.
get_intersection_points <- function(edges) {
  # make sure edges is an sf object, so st_intersection also returns origins
  intersections <- sf::st_intersection(sf::st_sf(edges)) |> suppressMessages()
  # only consider (multi-)point intersections
  points <- sf::st_collection_extract(intersections, type = "POINT")
  # cast multipoint intersections to points
  sfheaders::sf_cast(points, to = "POINT")
}

#' Insert intersection points into edges.
#'
#' @importFrom utils head tail
#'
#' @noRd
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `edges`. This is used when only
#'   geometry information is needed from that point onwards and all other
#'   attributes (i.e., columns) can be safely discarded. The object returned
#'   by `sf::st_geometry()` is a simple feature geometry list column of class
#'   `sfc`.
#' @srrstats {SP4.0, SP4.0a, SP4.1} The return value is of class
#'   [`sf::sfc_LINESTRING`] and it maintains the same units as the input.
insert_intersections <- function(edges, points, tol = 1.e-3) {

  edge_geometry <- sf::st_geometry(edges)
  edge_points <- sfheaders::sfc_to_df(edge_geometry)
  edge_ids <- edge_points$linestring_id
  edge_coords <- as.matrix(edge_points[c("x", "y")])

  # invert mapping from "intersection point" -> "edges" (list `origins` below)
  # to "edge" -> "intersection points" (`targets`). This way we can loop over
  # edges without having to get back to the same edge twice
  point_coords <- sf::st_coordinates(points)
  npoints <- nrow(points)
  origins <- points[["origins"]]
  targets <- split(rep(seq_len(npoints), lengths(origins)), unlist(origins))

  # create empty arrays where to store new edge coordinates and identifiers
  coords <- matrix(, nrow = 0, ncol = 2)
  linestring_ids <- c()

  for (edge_id in seq_along(edge_geometry)) {
    is_current_edge <- edge_ids == edge_id
    edge <- edge_coords[is_current_edge, ]
    for (point_id in targets[[edge_id]]) {
      point <- point_coords[point_id, ]
      if (is_point_in_edge(point, edge, tol = tol)) next
      # calculate distance from the target point to all edge points
      distances_from_target <- calc_distance(point, edge)
      # by computing the rolling sum over `distances` we get the summed
      # distances from the target to consecutive edge points
      summed_distances_from_target <- calc_rolling_sum(distances_from_target)
      # calculate length of line segments
      segment_lengths <- sqrt(apply(diff(edge)^2, sum, MARGIN = 1))
      # the point lies on the line segment for which the summed distances from
      # the endpoints is equal to the segment length
      index <- which(abs(summed_distances_from_target - segment_lengths) < tol)
      # update the coordinates by adding the target at the identified position
      edge <- rbind(head(edge, index),
                    point,
                    tail(edge, nrow(edge) - index))
    }
    coords <- rbind(coords, edge)
    linestring_ids <- c(linestring_ids, rep(edge_id, nrow(edge)))
  }

  edges_new <- sfheaders::sfc_linestring(
    data.frame(coords, linestring_id = linestring_ids),
    x = "x", y = "y", linestring_id = "linestring_id"
  )
  sf::st_crs(edges_new) <- sf::st_crs(edges)
  return(edges_new)
}

#' Check if a point is within a given edge.
#'
#' @noRd
is_point_in_edge <- function(point, edge, tol) {
  any(calc_distance(point, edge) < tol)
}

#' Calculate the distance from a point to an edge.
#'
#' @noRd
calc_distance <- function(point, edge) {
  sqrt((edge[, "x"] - point["X"]) ^ 2 + (edge[, "y"] - point["Y"]) ^ 2)
}

#' Calculate the rolling sum of a vector.
#'
#' @importFrom utils head tail
#'
#' @noRd
calc_rolling_sum <- function(x, n = 2) {
  cs <- cumsum(x)
  # roll the cumsum array by adding `n` zeros at its beginning and dropping
  # the last `n` elements
  cs_roll <- head(c(rep(0, n), cs), length(x))
  # the rolling sum is the difference between cumsum and its roll, after
  # dropping the first element of the array
  tail(cs - cs_roll, length(x) - 1)
}

#' Clean a spatial network.
#'
# nolint start
#' Subdivide edges by [adding missing nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#subdivide-edges),
#' (optionally) simplify the network (see [`simplify_network()`]), remove
#' [pseudo-nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#smooth-pseudo-nodes),
#' and discard all but the main connected component.
# nolint end
#'
#' @param network A network object of class [`sfnetworks::sfnetwork`]
#' @param simplify Whether the network should be simplified with
#'   [`simplify_network()`]
#'
#' @return A cleaned network object of class [`sfnetworks::sfnetwork`]
#' @export
#' @examplesIf interactive()
#' bucharest_osm <- get_osm_example_data()
#' edges <- dplyr::bind_rows(bucharest_osm$streets,
#'                           bucharest_osm$railways)
#' network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
#' clean_network(network)
#' @srrstats {G2.7} The `network` object provided as input must be of class
#'   `sfnetwork`. `sfnetwork` objects are `sf`-compatible and are commonly used
#'   for spatial network analysis.
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], same as the input class, explicitly documented
#'   as such, and it maintains the same units as the input.
clean_network <- function(network, simplify = TRUE) {
  # Check input
  checkmate::assert_class(network, "sfnetwork")
  checkmate::assert_logical(simplify, len = 1)

  # subdivide edges by adding missing nodes
  net <- tidygraph::convert(network, sfnetworks::to_spatial_subdivision,
                            .clean = TRUE) |> suppressWarnings()
  # run simplification steps
  if (simplify) net <- simplify_network(net)
  # remove pseudo-nodes
  net <- tidygraph::convert(net, sfnetworks::to_spatial_smooth, .clean = TRUE)
  # keep only the main connected component of the network
  net |>
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}

#' Simplify a spatial network by removing multiple edges and loops.
#'
# nolint start
#' Simplify the graph, removing loops and double-edge connections following
#' [this approach](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#simplify-network).
#' When dropping multiple edges, keep the shortest ones.
# nolint end
#'
#' @param network A network object
#'
#' @return A simplifed network object
#' @keywords internal
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], same as the input class, explicitly documented
#'   as such, and it maintains the same units as the input.
simplify_network <- function(network) {
  network |>
    sfnetworks::activate("edges") |>
    dplyr::arrange(sfnetworks::edge_length()) |>
    dplyr::filter(!tidygraph::edge_is_multiple()) |>
    dplyr::filter(!tidygraph::edge_is_loop())
}

#' Add weights to the network.
#'
#' This is to prepare the network for the search of shortest paths between node
#' pairs. The computed weights can account for edge lenghts, distance from a
#' target geometry, and whether or not an edge falls within a specified region,
#' which we aim to exclude from search of the shortest paths.
#'
#' For the i-th edge of the network, its weight \eqn{w_i} is defined in the
#' following way:
#' \deqn{
#'  w_i = |e_i| + d_{geom}(e_i) + p_{buf}(e_i)
#' }{wi = |ei| + d(geom, ei) + p(buffer, ei)}
#' where the first term is the edge length, the second one is the distance
#' from a target geometry (`target`, optional) and the last one is a penalty
#' that is added if the centroid of the edge falls within a specified region
#' (`exclude_area`, optional).
#'
#' Shortest paths calculated on the resulting network will thus tend to prefer
#' edges close to `target` and to avoid edges within `exclude_area`.
#'
#' @param network A network object
#' @param target Target geometry to calculate distances from, as a simple
#'   feature geometry
#' @param exclude_area Area that we aim to exclude from the shortest-path
#'   search, as a simple feature geometry
#' @param penalty Penalty (in the network CRS' units) that is added to the
#'   edges that falls within the excluded area
#' @param weight_name Name of the column in the edge table where to add the
#'   weights
#'
#' @return A network object of class [`sfnetworks::sfnetwork`] with weights
#'   added as a column in the edge table
#' @importFrom rlang :=
#' @keywords internal
#'
#' @srrstats {G2.4, G2.4b} Explicit conversion of logical vector to numeric with
#'   `as.numeric()` used for calculating penalty weights.
#' @srrstats {G2.7} The `network` object provided as input must be of class
#'   `sfnetwork`. `sfnetwork` objects are `sf`-compatible and are commonly used
#'   for spatial network analysis.
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `sf::st_as_sf(network, "edges")`.
#'   This is used when only geometry information is needed from that point
#'   onwards and all other attributes (i.e., columns) can be safely discarded.
#'   The object returned by `sf::st_geometry()` is a simple feature geometry
#'   list column of class `sfc`.
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], same as the input class, explicitly documented
#'   as such, and it maintains the same units as the input.
add_weights <- function(network, target = NULL, exclude_area = NULL,
                        penalty = 1000., weight_name = "weight") {
  edges <- sf::st_geometry(sf::st_as_sf(network, "edges"))
  lengths <- sf::st_length(edges)

  if (!is.null(target)) {
    distances <- sf::st_distance(edges, target, which = "Euclidean")
    distances <- drop(distances)  # drop column dimension with single element
  } else {
    distances <- 0.
  }
  distances <- set_units_like(distances, lengths)

  if (!is.null(exclude_area)) {
    is_inside <- edges |>
      sf::st_centroid() |>
      sf::st_intersects(exclude_area, sparse = FALSE) |>
      as.numeric()
    repellance <- penalty * is_inside
  } else {
    repellance <- 0.
  }
  repellance <- set_units_like(repellance, lengths)

  network |>
    tidygraph::activate("edges") |>
    dplyr::mutate(!!weight_name := lengths + distances + repellance)
}

#' Find shortest path between a pair of nodes in the network.
#'
#' @param network A spatial network object of class [`sfnetworks::sfnetwork`]
#' @param from Start node
#' @param to End node
#' @param weights Name of the column in the network edge table from where to
#'   take the weigths
#'
#' @return An [`sf::sfc_LINESTRING`] object
#' @importFrom rlang .data
#' @keywords internal
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `sf::st_as_sf(network, "edges")`.
#'   This is used when only geometry information is needed from that point
#'   onwards and all other attributes (i.e., columns) can be safely discarded.
#'   The object returned by `sf::st_geometry()` is a simple feature geometry
#'   list column of class `sfc`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sf::sfc_LINESTRING`], explicitly documented as such, and it maintains
#'   the same units as the input.
shortest_path <- function(network, from, to, weights = "weight") {
  paths <- sfnetworks::st_network_paths(
    network, from = from, to = to, weights = weights, type = "shortest",
  )
  edges <- sf::st_as_sf(network, "edges") |> sf::st_geometry()
  edge_path <- dplyr::pull(paths, .data$edge_paths) |> unlist()
  path <- edges[edge_path]
  if (length(path) > 1) {
    # if the path consists of multiple edges, merge them in a linestring
    path <- sf::st_union(path) |> sf::st_line_merge()
    # make sure the path direction is correct, reverse it otherwise
    start_pt <- lwgeom::st_startpoint(path)
    if (sf::st_distance(start_pt, from) > sf::st_distance(start_pt, to)) {
      path <- sf::st_reverse(path)
    }
  }
  path
}

#' Find the node in a network that is closest to a target geometry.
#'
#' @param network A network object of class [`sfnetworks::sfnetwork`]
#' @param target An object of class [`sf::sf`] or [`sf::sfc`]
#'
#' @return A node in the network as an object of class [`sf::sfc_POINT`]
#' @keywords internal
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `sf::st_as_sf(network, "nodes")`.
#'   This is used when only geometry information is needed from that point
#'   onwards and all other attributes (i.e., columns) can be safely discarded.
#'   The object returned by `sf::st_geometry()` is a simple feature geometry
#'   list column of class `sfc`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sf::sfc_POINT`], explicitly documented as such, and it maintains
#'   the same units as the input.
nearest_node <- function(network, target) {
  nodes <- sf::st_as_sf(network, "nodes") |>
    sf::st_geometry()
  idx <- sf::st_nearest_feature(target, nodes)
  nodes[idx]
}

#' Subset a network keeping the components that intersect a target geometry.
#'
#' If subsetting results in multiple disconnected components, we keep the main
#' one.
#'
#' @param network A spatial network object of class [`sfnetworks::sfnetwork`]
#' @param target The target geometry
#' @param elements The elements of the network to filter. It can be "nodes"
#'   or "edges"
#'
#' @return A spatial network object of class [`sfnetworks::sfnetwork`]
#' @importFrom rlang !!
#' @keywords internal
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sfnetworks::sfnetwork`], same as the input class, explicitly documented
#'   as such, and it maintains the same units as the input.
filter_network <- function(network, target, elements = "nodes") {
  if (elements == "nodes") {
    intersect_func <- sfnetworks::node_intersects
  } else if (elements == "edges") {
    intersect_func <- sfnetworks::edge_intersects
  } else {
    stop("Unknown elements - choose beetween 'nodes' and 'edges'")
  }
  network |>
    tidygraph::activate(!!elements) |>
    tidygraph::filter(intersect_func(target)) |>
    # keep only the main connected component of the network
    tidygraph::activate("nodes") |>
    dplyr::filter(tidygraph::group_components() == 1)
}

#' Identify network edges that are intersecting a geometry
#'
#' @param network A spatial network object of class [`sfnetworks::sfnetwork`]
#' @param geometry An object of class [`sf::sfc`]
#' @param index Whether to return the indices of the matchin edges or the
#'   geometries
#'
#' @return Indices or geometries of the edges intersecting the given geometry
#'   of class [`sf::sfc_LINESTRING`]
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} If `index = FALSE`, the return value
#'   is of class [`sf::sfc_LINESTRING`], explicitly documented as such, and it
#'   maintains the same units as the input.
get_intersecting_edges <- function(network, geometry, index = FALSE) {
  edges <- sf::st_as_sf(network, "edges")
  intersects <- sf::st_intersects(edges, geometry, sparse = FALSE)
  if (index) {
    which(intersects)
  } else {
    edges[intersects, ]
  }
}

#' Find intersections between the edges of two networks
#'
#' @param network_1,network_2 The two spatial network objects
#' @return An object of class [`sf::sfc`]
#' @keywords internal
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` objects `sf::st_as_sf(network_1, "edges")`
#'   and `sf::st_as_sf(network_2, "edges")`. This is used when only geometry
#'   information is needed from that point onwards and all other attributes
#'   (i.e., columns) can be safely discarded. The object returned by
#'   `sf::st_geometry()` is a simple feature geometry list column of class
#'   `sfc`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sf::sfc`], explicitly documented as such, and it maintains the same units
#'   as the input.
find_intersections <- function(network_1, network_2) {
  sf::st_intersection(sf::st_geometry(sf::st_as_sf(network_1, "edges")),
                      sf::st_geometry(sf::st_as_sf(network_2, "edges")))
}
