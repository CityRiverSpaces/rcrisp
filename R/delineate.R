#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected Coordinate Reference System (CRS) to use. If not
#'   provided, the suitable Universal Transverse Mercator (UTM) CRS is selected
#' @param bbox_buffer Add a buffer region to the city boundary to avoid edge
#'   effects close to its limits
#' @param initial_method The method employed to define the initial river
#'   corridor geometry. See [initial_corridor()] for the available methods
#' @param initial_buffer Buffer region to add to the river geometry to setup the
#'   initial corridor (only used if `initial_method` is `"buffer"`)
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `initial_method` is `"valley"`)
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e. to "cap" the corridor). See [cap_corridor()] for
#'   the available methods
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'  [get_segments()]  and [rcoins::stroke()]. Only used if `segments` is TRUE.
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#' @param force_download Download data even if cached data is available
#' @param ... Additional (optional) input arguments for retrieving the DEM
#'   dataset (see [get_dem()]). Only relevant if `initial_method` is `"valley"`
#'   and `dem` is NULL
#'
#' @return A list with the corridor, segments, and riverspace geometries
#' @export
delineate <- function(
  city_name, river_name, crs = NULL, bbox_buffer = NULL,
  initial_method = "valley", initial_buffer = NULL, dem = NULL,
  capping_method = "direct", angle_threshold = 90, corridor = TRUE,
  segments = FALSE, riverspace = FALSE, force_download = FALSE, ...
) {
  # Define the area of interest and (if not provided) the CRS
  bbox <- get_osm_bb(city_name)
  if (!is.null(bbox_buffer)) bbox <- buffer_bbox(bbox, buffer = bbox_buffer)
  if (is.null(crs)) crs <- get_utm_zone(bbox)

  # Retrieve OSM data needed for any delineation
  river <- get_osm_river(bb = bbox, river_name, crs = crs,
                         force_download = force_download)

  if (corridor) {
    # Retrieve the OSM data for corridor delineation
    streets <- get_osm_streets(bb = bbox, crs = crs,
                               force_download = force_download)
    railways <- get_osm_railways(bb = bbox, crs = crs,
                                 force_download = force_download)

    # If using the valley method, and the DEM is not provided, retrieve dataset
    if (initial_method == "valley" && is.null(dem)) {
      dem <- get_dem(bbox, crs = crs, force_download = force_download, ...)
    }

    # Set up the combined street and rail network for the delineation
    network_edges <- dplyr::bind_rows(streets, railways)
    network <- as_network(network_edges)

    # Run the corridor delineation on the spatial network
    bbox_repr <- reproject(bbox, crs)
    corridor <- delineate_corridor(
      network, river[1], river[2], bbox_repr,
      initial_method = initial_method, buffer = initial_buffer, dem = dem,
      capping_method = capping_method
    )
  } else {
    corridor <- NULL
  }

  if (segments) {
    if (is.null(corridor)) stop("Corridor geometry required for segmentation.")
    # Select the relevant part of the network
    buffer_corridor <- 100  # TODO should this be an additional input parameter?
    corridor_buffer <- sf::st_buffer(corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    segments <- delineate_segments(corridor, network_filtered,
                                   river[1], angle_threshold)
  } else {
    segments <- NULL
  }

  if (riverspace) {
    buildings <- get_osm_buildings(river = river, crs = crs,
                                   force_download = force_download)
    riverspace <- delineate_riverspace(buildings, river[2])
  } else {
    riverspace <- NULL
  }

  list(corridor = corridor, segments = segments, riverspace = riverspace)
}
