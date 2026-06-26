#' The delineation class
#'
#' An S3 class representing the result of a delineation as returned by
#' [delineate()].
#'
#' @name delineation
#'
#' @returns An object of class `delineation` containing zero or more of the
#'   spatial layers `$valley`, `$corridor`, `$segments`, and `$riverspace`
#'   (each an [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`] object), plus
#'   the base OSM and optionally DEM layers and an `$aoi` list with the
#'   delineation parameters produced by [define_aoi()]. See [delineate()] for
#'   details.
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' class(bd)
NULL

#' Delineate a corridor around a river
#'
#' Given a set of delineation parameters and input data, carry out corridor
#' delineation, corridor segmentation and/or riverspace delineation as indicated
#' by the user.
#'
#' The returned [`delineation`] class is a list wrapping objects of class
#' [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`] which can be retrieved
#' through subsetting and converted to other common spatial classes in typical
#' `sf`- or `terra`-based workflows.
#'
#' @param aoi A list of delineation parameters for an area of interest, namely
#'   `$city_name`, `$river_name`, `$crs`, `$network_buffer`, `$dem_buffer`, and
#'   `$buildings_buffer`. For more info see [define_aoi()].
#' @param osm A list with OpenStreetMap data sets within the `aoi`, as
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
#' @return A list object of class `delineation` containing zero or more of the
#'   following elements: "valley", "corridor", "segments", and "riverspace",
#'   each as an [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`] object
#'   (depending on the geometry of the input data). The list contains only
#'   the geometries corresponding to the delineation steps that were carried out
#'   (e.g., if `segments` is FALSE, the list will not contain a "segments"
#'   element). In any case, the returned object also contains the base layers
#'   and a list `aoi` with the parameters used for delineation.
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
#' @srrstats {SP2.0a, SP2.5a} The [delineation] class wraps [`sf::sfc_POLYGON`]
#'   or [`sf::sfc_MULTIPOLYGON`] objects retrievable by subsetting, which are
#'   directly compatible with `sf`- and `terra`-based conversion workflows
#'   (e.g. [sf::st_as_sf()], [terra::vect()]).
#' @srrstats {G2.3, G2.3a, G2.3b} The `checkmate` package is used to check that
#'   `corridor_init` only uses allowed values. The variable is also made
#'   case-independent with `tolower()`.
#' @srrstats {G2.6} One-dimensional distance input is pre-processed by
#'   `preprocess_distance()` to handle `units` objects or other vector-like
#'   classes with storage mode `numeric`.
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is a list of
#'   [`sf::sfc_POLYGON`] objects, explicitly documented as such, and it
#'   maintains the same units as the input.
delineate <- function(
  aoi, osm, dem = NULL, corridor_init = "valley",
  max_iterations = 10, capping_method = "shortest-path", angle_threshold = 100,
  corridor = TRUE, segments = FALSE, riverspace = FALSE
) {
  # Pre-process distances
  if (!is.null(network_buffer)) {
    network_buffer   <- preprocess_distance(network_buffer)
  }
  if (!is.null(buildings_buffer)) {
    buildings_buffer <- preprocess_distance(buildings_buffer)
  }
  dem_buffer <- preprocess_distance(dem_buffer)
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

  delineations$aoi <- aoi

  class(delineations) <- "delineation"
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

#' Plot a delineation object
#'
#' This S3 method provides a way to quickly visualise how the layers of a
#' delineation object fit together using base R graphics. A delineation object
#' typically includes the base layers `streets`, `railways`,
#' `river_centerline` and `river_surface`, and the delineations of the
#' `valley`, `corridor`, `segments`, and `riverspace`. Depending on the
#' delineation object, some of the delineation layers may not be present and
#' thus will not be plotted.
#'
#' @param x An object of class [delineation], which is typically the output of
#'   the [delineate()] function.
#' @param ... Not used; included for compatibility with the generic.
#' @param legend logical. If TRUE (default), a legend is added to the plot.
#'
#' @returns `NULL`, invisibly. This function is called for its side effect of
#'   producing a plot.
#' @seealso [geom_delineation()] for a `ggplot2`-based alternative.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' plot(bd)
#'
#' @srrstats {SP2.0b} If object of class other than `delineation` is provided
#'   as input, the function raises an error with an informative message.
#' @srrstats {SP5.0} A `plot()` method is implemented for the quick
#'   visualisation of ojects of class [delineation].
#' @srrstats {SP5.1} Spatial layers are plotted using [sf::plot.sfc_POLYGON()]
#'   or [sf::plot.sfc_MULTIPOLYGON()], which maps easting to the x-axis and
#'   northing to the y-axis preserving the correct orientation and aspect ratio
#'   of the CRS.
#' @srrstats {SP5.2} Axis labels show the units of the coordinate reference
#'   system, derived from [sf::st_crs()].
plot.delineation <- function(x, ..., legend = TRUE) {
  if (!inherits(x, "delineation")) {
    stop("The object is not of class 'delineation'")
  }
  x <- unclass(x)

  # Set plot extent with first layer. If corridor is missing, then segments is
  # missing too, so only riverspace is checked additionally.
  if (!is.null(x$corridor)) {
    plot(x$corridor, col = NA, border = NA)
  } else if (!is.null(x$riverspace)) {
    plot(x$riverspace, col = NA, border = NA)
  } else {
    stop("No delineation layers present in the delineation object.")
  }

  base_layers <- c("streets", "railways", "river_centerline")
  if (!all(base_layers %in% names(x))) {
    warning(paste("Not all base layers found in the delineation object.",
                  "Plotting without those."))
  }

  if (!is.null(x$valley)) {
    plot(x$valley, col = "grey70", border = NA, add = TRUE)
  }
  if (!is.null(x$riverspace)) {
    plot(x$riverspace, col = "lightblue", border = NA, add = TRUE)
  }
  if (!is.null(x$river_surface)) {
    plot(x$river_surface, col = "blue", border = NA, add = TRUE)
  }
  if (!is.null(x$river_centerline)) {
    plot(x$river_centerline, col = "blue", add = TRUE)
  }
  if (!is.null(x$railways)) {
    plot(x$railways$geometry, add = TRUE, lwd = 0.5, col = "grey80")
  }
  if (!is.null(x$streets)) {
    plot(x$streets$geometry, add = TRUE, lwd = 0.5, col = "grey50")
  }
  if (!is.null(x$segments)) {
    plot(x$segments, border = "red", add = TRUE, lwd = 1)
  }
  if (!is.null(x$corridor)) {
    plot(x$corridor, border = "red", add = TRUE, lwd = 3)
  }

  if (!is.null(x$aoi$city_name) && !is.null(x$aoi$river_name)) {
    graphics::title(main = paste0("City: ", x$aoi$city_name, "\n",
                                  "River: ", x$aoi$river_name))
  }
  graphics::title(xlab = paste0("Easting (m)"),
                  ylab = paste0("Northing (m)"))

  if (legend) {
    leg <- data.frame(
      label  = c("River",  "Valley",  "Riverspace", "Corridor", "Segment"),
      pch    = c(15,       15,        15,            NA,         NA),
      pt.cex = c(2,        2,         2,             NA,         NA),
      lty    = c(NA,       NA,        NA,            1,          1),
      lwd    = c(NA,       NA,        NA,            3,          1),
      col    = c("blue",   "grey70",  "lightblue",   "red",      "red"),
      stringsAsFactors = FALSE
    )
    present <- c(
      !is.null(x$river_surface),
      !is.null(x$valley),
      !is.null(x$riverspace),
      !is.null(x$corridor),
      !is.null(x$segments)
    )
    leg <- leg[present, ]
    if (nrow(leg) > 0) {
      graphics::legend(
        "bottomleft", inset = 0.01, y.intersp = 0.8, seg.len = 1,
        legend = leg$label,
        pch    = leg$pch,
        pt.cex = leg$pt.cex,
        lty    = leg$lty,
        lwd    = leg$lwd,
        col    = leg$col
      )
    }
  }

  invisible(NULL)
}

#' Print a delineation object
#'
#' Prints a compact summary of a [delineation] object, showing which layers
#' are present and their feature counts.
#'
#' @param x An object of class [delineation].
#' @param ... Not used; included for compatibility with the generic.
#'
#' @returns `x`, invisibly.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' print(bd)
#'
#' @srrstats {SP2.0b} If object of class other than `delineation` is provided
#'   as input, the function raises an error with an informative message.
print.delineation <- function(x, ...) {
  if (!inherits(x, "delineation")) {
    stop("'x' must be object of class 'delineation'.")
  }

  d <- unclass(x)

  .delineation_header(d)

  cat("\nDelineation layers:\n")
  for (layer in c("valley", "corridor", "segments", "riverspace")) {
    val <- d[[layer]]
    if (!is.null(val)) {
      n <- length(val)
      cat(sprintf("  $%-15s %d feature%s\n", layer, n, if (n == 1) "" else "s"))
    } else {
      cat(sprintf("  $%-15s -\n", layer))
    }
  }

  cat("\nBase layers:\n")
  for (layer in c("streets", "railways", "river_centerline", "river_surface")) {
    val <- d[[layer]]
    if (!is.null(val)) {
      n <- if (inherits(val, "sf")) nrow(val) else length(val)
      cat(sprintf("  $%-15s %d feature%s\n", layer, n, if (n == 1) "" else "s"))
    } else {
      cat(sprintf("  $%-15s -\n", layer))
    }
  }

  invisible(x)
}

#' Summarise a delineation object
#'
#' Computes summary statistics for a `delineation` object and returns them as
#' a structured list of class `summary.delineation`. Delineation layers are
#' reported with their area (in km²). For `segments`, the number of features
#' and mean area are also given. Base layers are reported with feature counts;
#' `river_centerline` as total length (in km) and `river_surface` as area
#' (in km²).
#'
#' @param object An object of class `delineation`.
#' @param ... Not used; included for compatibility with the generic.
#'
#' @returns An object of class `summary.delineation`, which is a named list
#'   with elements `city_name`, `river_name`, `crs`, `parameters`,
#'   `delineation_layers`, and `base_layers`. `parameters` contains
#'   `network_buffer`, `dem_buffer`, and `buildings_buffer` (in metres), or
#'   `NULL` if no `aoi` was stored. Each layer entry is itself a named list
#'   of stats, or `NULL` if the layer is absent.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' s <- summary(bd)
#' s$delineation_layers$corridor$area_km2
#'
#' @srrstats {SP2.0b} If object of class other than `delineation` is provided
#'   as input, the function raises an error with an informative message.
summary.delineation <- function(object, ...) {
  if (!inherits(object, "delineation")) {
    stop("'object' must be object of class 'delineation'.")
  }

  d <- unclass(object)

  delineation_layers <- list(
    valley = if (!is.null(d$valley)) {
      list(area_km2 = round(
        sum(as.numeric(sf::st_area(d$valley))) / 1e6, 1
      ))
    },
    corridor = if (!is.null(d$corridor)) {
      list(area_km2 = round(
        sum(as.numeric(sf::st_area(d$corridor))) / 1e6, 1
      ))
    },
    segments = if (!is.null(d$segments)) {
      n <- length(d$segments)
      total_area_km2 <- round(
        sum(as.numeric(sf::st_area(d$segments))) / 1e6, 1
      )
      list(n = n, total_area_km2 = total_area_km2,
           mean_area_km2 = round(total_area_km2 / n, 1))
    },
    riverspace = if (!is.null(d$riverspace)) {
      list(area_km2 = round(
        sum(as.numeric(sf::st_area(d$riverspace))) / 1e6, 1
      ))
    }
  )

  base_layers <- list(
    streets = if (!is.null(d$streets)) list(n = nrow(d$streets)),
    railways = if (!is.null(d$railways)) list(n = nrow(d$railways)),
    river_centerline = if (!is.null(d$river_centerline)) {
      list(length_km = round(
        sum(as.numeric(sf::st_length(d$river_centerline))) / 1e3, 1
      ))
    },
    river_surface = if (!is.null(d$river_surface)) {
      list(area_km2 = round(
        sum(as.numeric(sf::st_area(d$river_surface))) / 1e6, 1
      ))
    }
  )

  parameters <- if (!is.null(d$aoi)) {
    list(
      network_buffer = d$aoi$network_buffer,
      dem_buffer = d$aoi$dem_buffer,
      buildings_buffer = d$aoi$buildings_buffer
    )
  }

  structure(
    list(
      city_name = d$aoi$city_name,
      river_name = d$aoi$river_name,
      crs = .delineation_crs(d),
      parameters = parameters,
      delineation_layers = delineation_layers,
      base_layers = base_layers
    ),
    class = "summary.delineation"
  )
}

#' Print a summary.delineation object
#'
#' @param x An object of class `summary.delineation`, as returned by
#'   [summary.delineation()].
#' @param ... Not used; included for compatibility with the generic.
#'
#' @returns `x`, invisibly.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' s <- summary(bd)
#' print(s)
#'
#' @srrstats {SP2.0b} If object of class other than `summary.delineation` is
#'   provided as input, the function raises an error with an informative
#'   message.
print.summary.delineation <- function(x, ...) {
  if (!inherits(x, "summary.delineation")) {
    stop("'x' must be object of class 'summary.delineation'.")
  }

  if (!is.null(x$city_name) && !is.null(x$river_name)) {
    cat("Delineation:", x$city_name, "-", x$river_name, "\n")
  } else {
    cat("Delineation\n")
  }
  if (!is.na(x$crs)) cat("CRS:        ", x$crs, "\n")

  if (!is.null(x$parameters)) {
    cat("\nDelineation parameters:\n")
    param_labels <- c(
      network_buffer = "network_buffer",
      dem_buffer = "dem_buffer",
      buildings_buffer = "buildings_buffer"
    )
    for (p in names(param_labels)) {
      val <- x$parameters[[p]]
      if (!is.null(val)) {
        cat(sprintf("  %-16s %g m\n", param_labels[[p]], val))
      }
    }
  }

  cat("\nDelineation layers:\n")
  for (layer in c("valley", "corridor", "segments", "riverspace")) {
    val <- x$delineation_layers[[layer]]
    if (!is.null(val)) {
      if (!is.null(val$n)) {
        cat(sprintf(
          "  $%-15s %d features, total %.1f km\u00b2 (mean %.1f km\u00b2)\n",
          layer, val$n, val$total_area_km2, val$mean_area_km2
        ))
      } else {
        cat(sprintf("  $%-15s %.1f km\u00b2\n", layer, val$area_km2))
      }
    } else {
      cat(sprintf("  $%-15s -\n", layer))
    }
  }

  cat("\nBase layers:\n")
  for (layer in c("streets", "railways")) {
    val <- x$base_layers[[layer]]
    if (!is.null(val)) {
      cat(sprintf("  $%-15s %d feature%s\n", layer, val$n,
                  if (val$n == 1) "" else "s"))
    }
  }
  if (!is.null(x$base_layers$river_centerline)) {
    cat(sprintf("  $%-15s %.1f km\n", "river_centerline",
                x$base_layers$river_centerline$length_km))
  }
  if (!is.null(x$base_layers$river_surface)) {
    cat(sprintf("  $%-15s %.1f km\u00b2\n", "river_surface",
                x$base_layers$river_surface$area_km2))
  }

  invisible(x)
}

#' @noRd
.delineation_header <- function(d) {
  if (!is.null(d$aoi$city_name) && !is.null(d$aoi$river_name)) {
    cat("City:    ", d$aoi$city_name, "\n",
        "River:   ", d$aoi$river_name, "\n",
        sep = "")
  } else {
    cat("Delineation\n")
  }

  crs_name <- .delineation_crs(d)
  if (!is.na(crs_name)) cat("CRS:    ", crs_name, "\n")
}

#' @noRd
.delineation_crs <- function(d) {
  layers <- c(
    "corridor", "valley", "segments", "riverspace",
    "river_centerline", "river_surface", "streets", "railways"
  )
  for (layer in layers) {
    val <- d[[layer]]
    if (!is.null(val)) {
      crs <- sf::st_crs(val)
      if (!is.na(crs)) return(crs$Name)
    }
  }
  NA_character_
}
