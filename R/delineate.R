#' Delineate a corridor around a river
#'
#' @param aoi A list of delineation parameters, namely `$city_name`,
#'   `$river_name`, `$bb`, `$crs`, `$network_buffer`, `$dem_buffer`, and
#'   `$buildings_buffer`. For more info see [define_aoi()].
#' @param osm A list with OpenStreetMap data sets for the a location, as
#'   objects of class [`sf::sfc`]
#' @param dem Digital elevation model (DEM) of the region (only used if
#'   `corridor_init` is `"valley"`)
#' @param corridor_init How to estimate the initial guess of the river corridor.
#'   It can take the following values:
#'   * "valley": use the river valley boundary, as estimated from a Digital
#'     Elevation Model (DEM) (for more info see [delineate_valley()])
#'   * numeric or integer: use a buffer region of the given size (in meters)
#'     around the river centerline
#'   * An [`sf::sf`] or [`sf::sfc`] object: use the given input geometry
#' @param max_iterations Maximum number of iterations employed to refine the
#'   corridor edges (see [`corridor_edge()`]).
#' @param capping_method The method employed to connect the corridor edge end
#'   points (i.e., to "cap" the corridor), as character vector of length one.
#'   See [cap_corridor()] for the available methods.
#' @param angle_threshold Only network edges forming angles above this threshold
#'   (in degrees) are considered when forming segment edges. See
#'   [delineate_segments()] and [rcoins::stroke()]. Only used if `segments` is
#'   TRUE.
#' @param corridor Whether to carry out the corridor delineation
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#'
#' @return A list containing zero or more of the following elements: "valley",
#'   "corridor", "segments", and "riverspace", each as an [`sf::sfc_POLYGON`] or
#'   [`sf::sfc_MULTIPOLYGON`] object (depending on the geometry of the input
#'   data). The list contains only the geometries corresponding to the
#'   delineation steps that were carried out (e.g., if `segments` is FALSE, the
#'   list will not contain a "segments" element).
#' @export
#' @examplesIf interactive()
#' # Define delineation parameters within area of interest
#' aoi <- define_aoi("Bucharest", "Dâmbovița")
#'
#' # Get data
#' osm <- get_osm(aoi)
#' dem <- get_dem(aoi, osm)
#'
#' # Delineate with defaults
#' delineate(aoi, osm, dem)
#'
#' # Carry out all delineations
#' delineate(aoi, osm, dem, segments = TRUE, riverspace = TRUE)
#' @srrstats {G2.3, G2.3a, G2.3b} The `checkmate` package is used to check that
#'   `corridor_init` only uses allowed values. The variable is also made
#'   case-independent with `tolower()`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is a list of
#'   [`sf::sfc_POLYGON`] objects, explicitly documented as such, and it
#'   maintains the same units as the input.
delineate <- function(
  aoi, osm, dem = NULL, corridor_init = "valley",
  max_iterations = 10, capping_method = "shortest-path", angle_threshold = 100,
  corridor = TRUE, segments = FALSE, riverspace = FALSE
) {
  # Check input
  checkmate::assert_list(aoi)
  if (is.character(corridor_init)) {
    corridor_init <- tolower(corridor_init)
    checkmate::assert_choice(corridor_init, c("valley"))
  }
  checkmate::assert_logical(corridor, len = 1)
  checkmate::assert_logical(segments, len = 1)
  checkmate::assert_logical(riverspace, len = 1)

  delineations <- list()

  if (segments && !corridor) stop("Segmentation requires corridor delineation.")

  delineations$streets <- osm$streets
  delineations$railways <- osm$railways
  delineations$river_centerline <- osm$river_centerline
  delineations$river_surface <- osm$river_surface

  # Carry out the required delineations
  if (corridor) {
    # If using the valley method, the user must provide a DEM
    if (corridor_init == "valley") {
      if (is.null(dem)) {
        stop("If initial corridor is \"valley\", a DEM must be provided.")
      }
      corridor_init <- delineate_valley(dem, osm$river_centerline)
      delineations$valley <- corridor_init
    } else {
      corridor_init <- corridor_init
    }

    if (is.null(osm$streets) || is.null(osm$railways)) {
      stop(paste0("Spatial network (streets, railways) data is not available. ",
                  "Did you set `network = FALSE` when retrieving OSM data ",
                  "with `get_osm()`?"))
    }

    # Set up the combined street and rail network for the delineation
    network_edges <- dplyr::bind_rows(osm$streets, osm$railways)
    network <- as_network(network_edges)

    # Run the corridor delineation on the spatial network
    delineations$corridor <- delineate_corridor(
      network, osm$river_centerline, max_width = aoi$network_buffer,
      corridor_init = corridor_init, max_iterations = max_iterations,
      capping_method = capping_method
    )
  }

  if (segments) {
    # Select the relevant part of the network
    buffer_corridor <- 100
    corridor_buffer <- sf::st_buffer(delineations$corridor, buffer_corridor)
    network_filtered <- filter_network(network, corridor_buffer)

    delineations$segments <- delineate_segments(delineations$corridor,
                                                network_filtered,
                                                osm$river_centerline,
                                                angle_threshold)
  }

  if (riverspace) {
    if (is.null(osm$aoi_buildings)) {
      stop(paste0("AOI for buildings is not available. ",
                  "Did you set `buildings = FALSE` when retrieving OSM data ",
                  "with `get_osm()`?"))
    }
    river_centerline_clipped <- sf::st_intersection(
      osm$river_centerline, osm$aoi_buildings |>
        sf::st_transform(aoi$crs)
    )
    river_combined <- combine_river_features(river_centerline_clipped,
                                             osm$river_surface)
    delineations$riverspace <- delineate_riverspace(river_combined,
                                                    osm$buildings)
  }

  class(delineations) <- c("delineation", "list")
  delineations
}

#' Delineate a corridor around a river
#'
#' This is a convenience function used for quick delineation. With only the city
#' name and river name as input, it uses default delineation parameters, it
#' retrieves OSM and DEM data and returns a list with all three delineations.
#'
#' @param city_name A character vector of length one.
#' @param river_name A character vector of length one.
#' @param corridor Whether to carry out the corridor delineation. Default is
#'   TRUE.
#' @param segments Whether to carry out the corridor segmentation.
#'   Default is TRUE.
#' @param riverspace Whether to carry out the riverspace delineation.
#'   Default is TRUE.
#'
#' @returns A list with the valley, corridor, segments, and riverspace
#'   geometries as [`sf::sfc_POLYGON`] objects.
#' @export
#'
#' @examplesIf interactive()
#' delineate_city_river("Bucharest", "Dâmbovița")
delineate_city_river <- function(city_name, river_name,
                                 corridor = TRUE,
                                 segments = TRUE,
                                 riverspace = TRUE) {
  aoi <- define_aoi(city_name, river_name)
  osm <- get_osm(aoi)
  dem <- get_dem(aoi, osm)
  delineate(aoi, osm, dem,
            corridor = corridor,
            segments = segments,
            riverspace = riverspace)
}
