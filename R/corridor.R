#' Delineate a river corridor on a spatial network.
#'
#' @param network The spatial network to be used for the delineation
#' @param river_centerline A simple feature geometry representing the river
#'   centerline
#' @param river_surface A simple feature geometry representing the river surface
#' @param bbox Bounding box defining the extent of the area of interest
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param buffer Buffer region to add to the river geometry to setup the initial
#'   corridor (only used if `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#'
#' @return A simple feature geometry representing the river corridor
#' @export
get_corridor <- function(
  network, river_centerline, river_surface, bbox, initial_method = "valley",
  buffer = NULL, dem = NULL, capping_method = "direct"
) {
  # Drop all features of river centerline and surface but the geometries
  river_centerline <- sf::st_geometry(river_centerline)
  river_surface <- sf::st_geometry(river_surface)

  # Draw the initial corridor geometry within the area of interest
  river <- c(river_centerline, river_surface)
  corridor_init <- initial_corridor(river, method = initial_method,
                                    buffer = buffer, dem = dem, bbox = bbox)

  # Pick the corridor end points
  end_points <- corridor_end_points(river_centerline, network, aoi = bbox)

  # Split the area of interest along the river centerline
  regions <- split_aoi(bbox, river_centerline)

  # Determine the initial corridor edges by splitting the initial corridor
  # boundary through the just determined regions
  edges_init <- initial_edges(corridor_init, regions)

  # Run the delineation, setting the initial corridor geometry as excluded area
  network_1 <- filter_network(network, regions[1])
  edge_1 <- corridor_edge(network_1, end_points, edges_init[1], corridor_init)
  network_2 <- filter_network(network, regions[2])
  edge_2 <- corridor_edge(network_2, end_points, edges_init[2], corridor_init)

  # Run the delineation again, with the previously determined edges as targets.
  # This refinement step has the effect of "smoothening" the edges
  edge_1 <- corridor_edge(network_1, end_points, edge_1)
  edge_2 <- corridor_edge(network_2, end_points, edge_2)

  cap_corridor(c(edge_1, edge_2), capping_method, network)
}

#' Draw the initial geometry of a river corridor.
#'
#' @param river A simple feature geometry representing the river
#' @param method The method employed to draw the initial river corridor:
#'   - "buffer": add a fixed buffer region to the river geometry (see
#'     [river_buffer()])
#'   - "valley" (default): use the river valley boundary, as estimated from the
#'     provided digital elevation model (DEM, see [get_valley()] for details on
#'     the implementation)
#' @param buffer Buffer region to add to the river geometry (only used if
#'   `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
initial_corridor <- function(
  river, method = "valley", buffer = NULL, dem = NULL, bbox = NULL
) {
  if (method == "buffer") {
    if (is.null(buffer)) {
      stop("Buffer should be provided if `method` is `'buffer'`")
    }
    river_buffer(river, buffer, bbox = bbox)
  } else if (method == "valley") {
    if (is.null(dem)) {
      stop("DEM should be provided if `method` is `'valley'`")
    }
    get_valley(dem, river, bbox = bbox)
  } else {
    stop(
      sprintf("Unknown method to initialize river corridor: %s", method)
    )
  }
}

#' Draw a corridor as a fixed buffer region around a river.
#'
#' The river geometry may consist of multiple spatial features, these are merged
#' after applying the buffer.
#'
#' @param river A simple feature geometry representing the river
#' @param buffer Size of the buffer (in the river's CRS units)
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return A simple feature geometry
river_buffer <- function(river, buffer, bbox = NULL) {
  river_buf <- sf::st_buffer(river, buffer)
  river_buf_union <- sf::st_union(river_buf)
  if (!is.null(bbox)) {
    sf::st_crop(river_buf_union, bbox)
  } else {
    river_buf_union
  }
}

#' Find the corridor end points.
#'
#' Determine the extremes (start and end point) of the river corridor
#' using the river center line and the spatial network used for the delineation.
#' The end points are selected as the most external river crossing edges of the
#' network within the area of interest. Note that if the river intersects the
#' area of interest multiple times, only the longest intersecting segment is
#' considered here.
#'
#' @param river A simple feature geometry representing the river centerline
#' @param network The spatial network used for the delineation, either provided
#'   as a [`sfnetworks::sfnetwork`] or as a [`sf::sf`]/[`sf::sfc`] object (edges
#'   only)
#' @param aoi Area of interest, provided as a bounding box or as polygon. If not
#'   given, it is taken as the bounding box of the spatial network
#'
#' @return A simple feature geometry including a pair of points
corridor_end_points <- function(river, network, aoi = NULL) {
  # Only consider the edges of the spatial network
  spat_net_edges <- sf::st_geometry(sf::st_as_sf(network, "edges"))

  # Set area of interest as a polygon, even if not defined
  if (is.null(aoi)) aoi <- sf::st_bbox(spat_net_edges)
  aoi <- as_sfc(aoi)

  # Clip the river geometry using the area of interest. This might lead to
  # multiple river segments
  river_segments <- sf::st_intersection(aoi, sf::st_cast(river, "LINESTRING"))

  # Build a spatial network using the river segments and select the connected
  # component with overall longest edge length
  river_network <- as_network(river_segments, flatten = FALSE, clean = FALSE)
  components <- tidygraph::morph(river_network, tidygraph::to_components)
  sum_edge_lengths <- function(x) sum(sf::st_length(sf::st_as_sf(x, "edges")))
  edge_lengths <- sapply(components, sum_edge_lengths)
  river_network <- components[[which.max(edge_lengths)]]
  river_network <- clean_network(river_network)

  # Find intersections with the spatial network, and add these points as nodes
  # to the river network. These nodes are the candidates corridor end points
  river_network_edges <- sf::st_geometry(sf::st_as_sf(river_network, "edges"))
  intersections <- sf::st_intersection(river_network_edges, spat_net_edges)
  river_network <- sfnetworks::st_network_blend(river_network, intersections)
  nodes <- nearest_node(river_network, intersections)

  # Pick a reference point on the selected river segment, either up- or down-
  # stream from all crossings. Pick this as the closest node to the aoi boundary
  reference <- nearest_node(river_network, sf::st_boundary(aoi))

  # Compute the network distance from the reference node to all candidate end
  # points. The closest and furthest nodes are selected as corridor end points
  river_network <- add_weights(river_network, weight_name = "weight")
  distances <- sfnetworks::st_network_cost(river_network, from = reference,
                                           to = nodes, weights = "weight")
  end_points <- c(nodes[which.min(distances[1, ])],
                  nodes[which.max(distances[1, ])])
  if (end_points[1] == end_points[2]) {
    stop("Corridor start- and end-points coincide!")
  }
  end_points
}

#' Split the area of interest (AoI) by a river.
#'
#' Return the fragments produced. If more than two fragments are obtained, only
#' return the two largest fragments.
#'
#' @param bbox Bounding box defining the extent of the area of interest
#' @param river A simple feature geometry representing the river centerline
#'
#' @return A simple feature geometry set of two areas of interest
#' @importFrom rlang .data
split_aoi <- function(bbox, river) {
  regions <- split(sf::st_as_sfc(bbox), river)

  if (length(regions) > 2) {
    # Sort fragments according to area in descending order
    regions_sorted <- sf::st_as_sf(regions) |>
      dplyr::mutate(area = sf::st_area(regions)) |>
      dplyr::arrange(-.data$area)

    # Return the geometries of the two largest fragments
    sf::st_geometry(regions_sorted[1:2, ])
  } else {
    regions
  }
}

#' Identify the initial edges of the river corridor
#'
#' These are defined by splitting the initial corridor boundary into the
#' sub-regions that the river forms in the area of interest
#'
#' @param corridor_initial A simple feature geometry representing the area of
#'   the initial corridor
#' @param regions A simple feature geometry representing the sub-regions formed
#'   by cutting the area of interest along the river
#'
#' @return A simple feature geometry representing the initial corridor edges
initial_edges <- function(corridor_initial, regions) {
  corridor_split <- sf::st_intersection(regions, corridor_initial)
  boundaries <- sf::st_union(sf::st_boundary(regions))
  sf::st_difference(sf::st_boundary(corridor_split), boundaries)
}

#' Draw a corridor edge on the spatial network.
#'
#' @param network The spatial network used for the delineation
#' @param end_points Target start- and end-point
#' @param target_edge Target edge geometry to follow in the delineation
#' @param exclude_area Region that we aim to exclude from the delineation
#'
#' @return A simple feature geometry representing the edge (i.e. a linestring)
corridor_edge <- function(
  network, end_points, target_edge, exclude_area = NULL
) {
  nodes <- nearest_node(network, end_points)
  network <- add_weights(network, target_edge, exclude_area)
  shortest_path(network, from = nodes[1], to = nodes[2])
}

#' Cap the corridor by connecting the edge end points
#'
#' @param edges A simple feature geometry representing the corridor edges
#' @param method The method employed for the capping:
#'   - `direct` (default): connect the start points and the end points of the
#'     edges via straight segments
#'   - `shortest-path`: find the network-based shortest-path connections
#'     between the edge end points.
#' @param network A spatial network object, only required if
#'   `method = 'shortest-path'`
#'
#' @return A simple feature geometry representing the corridor (i.e. a polygon)
cap_corridor <- function(edges, method = "direct", network = NULL) {

  start_pts <- lwgeom::st_startpoint(edges)
  end_pts <- lwgeom::st_endpoint(edges)

  if (method == "direct") {
    cap_edge_1 <- as_linestring(start_pts)
    cap_edge_2 <- as_linestring(end_pts)
  } else if (method == "shortest-path") {
    if (is.null(network)) stop(
      "A network should be provided if `capping_method = 'shortest-path'`"
    )
    network <- add_weights(network)
    cap_edge_1 <- shortest_path(network, from = start_pts[1], to = start_pts[2])
    cap_edge_2 <- shortest_path(network, from = end_pts[1], to = end_pts[2])
    # TODO: raise warning if lenght is 2 times longer than direct segment
  } else {
    stop(
      sprintf("Unknown method to cap the river corridor: {method}", method)
    )
  }
  as_polygon(c(edges, cap_edge_1, cap_edge_2))
}
