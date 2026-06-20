#' Print a delineation object
#'
#' Prints a compact summary of a [delineation] object, showing which layers
#' are present and their feature counts.
#'
#' @param x An object of class [delineation].
#'
#' @returns `x`, invisibly.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' print(bd)
print.delineation <- function(x) {
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
#' @param x An object of class `delineation`.
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
summary.delineation <- function(x) {

  d <- unclass(x)

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
#'
#' @returns `x`, invisibly.
#' @export
print.summary.delineation <- function(x) {
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
