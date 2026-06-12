#' Delineate a corridor around a river
#'
#' @param aoi A list of delineation parameters
#' @param osm_data A list with OpenStreetMap data sets for the a location, as
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
#' @return A list with the valley, corridor, segments, and riverspace geometries
#'   as [`sf::sfc_POLYGON`] objects.
#' @export
#' @examplesIf interactive()
#' # Define delineation parameters within area of interest
#' aoi <- define_aoi("Bucharest", "Dâmbovița")
#'
#' # Get data
#' osm <- get_osmdata(aoi)
#' dem <- get_dem(aoi, osm_data)
#'
#' # Delineate with defaults
#' delineate(aoi, osm_data, dem)
#'
#' # Carry out all delineations
#' delineate(aoi, osm_data, dem, segments = TRUE, riverspace = TRUE)
#' @srrstats {G2.3, G2.3a, G2.3b} The `checkmate` package is used to check that
#'   `corridor_init` only uses allowed values. The variable is also made
#'   case-independent with `tolower()`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is a list of
#'   [`sf::sfc_POLYGON`] objects, explicitly documented as such, and it
#'   maintains the same units as the input.
delineate <- function(
  aoi, osm_data, dem = NULL, corridor_init = "valley",
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

  # Carry out the required delineations
  if (corridor) {
    # If using the valley method, the user must provide a DEM
    if (corridor_init == "valley") {
      if (is.null(dem)) {
        stop("If initial corridor is \"valley\", a DEM must be provided.")
      }
      corridor_init <- delineate_valley(dem, osm_data$river_centerline)
      delineations$valley <- corridor_init
    } else {
      corridor_init <- corridor_init
    }

    # Set up the combined street and rail network for the delineation
    network_edges <- dplyr::bind_rows(osm_data$streets, osm_data$railways)
    network <- as_network(network_edges)

    # Run the corridor delineation on the spatial network
    delineations$corridor <- delineate_corridor(
      network, osm_data$river_centerline, max_width = aoi$network_buffer,
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
                                                osm_data$river_centerline,
                                                angle_threshold)
  }

  if (riverspace) {
    river_centerline_clipped <- sf::st_intersection(
      osm_data$river_centerline, osm_data$aoi_buildings |>
        sf::st_transform(aoi$crs)
    )
    river_combined <- combine_river_features(river_centerline_clipped,
                                             osm_data$river_surface)
    delineations$riverspace <- delineate_riverspace(river_combined,
                                                    osm_data$buildings)
  }

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
  osm_data <- get_osmdata(aoi)
  dem <- get_dem(aoi, osm_data)
  delineate(aoi, osm_data, dem,
            corridor = corridor,
            segments = segments,
            riverspace = riverspace)
}
