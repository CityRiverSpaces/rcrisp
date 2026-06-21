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
    graphics::legend(
      "bottomleft", inset = 0.01, y.intersp = 0.8, seg.len = 1,
      legend = c("River", "Valley", "Riverspace", "Corridor", "Segment"),
      pch    = c(15,      15,       15,           NA,         NA),
      pt.cex = c(2,       2,        2,            NA,         NA),
      lty    = c(NA,      NA,       NA,           1,          1),
      lwd    = c(NA,      NA,       NA,           3,          1),
      col    = c("blue",  "grey70", "lightblue",  "red",      "red")
    )
  }

  invisible(NULL)
}

#' ggplot2 layer for a delineation object
#'
#' Creates a list of [ggplot2::geom_sf()] layers representing the delineation
#' results of a delineation object. The list can be added to a
#' [ggplot2::ggplot()] object with `+`, and further layers or themes can be
#' added on top. Base layers (`streets`, `railways`, `river_centerline`,
#' `river_surface`) are not included and can be added separately with
#' [ggplot2::geom_sf()].
#'
#' The following layers are included (where present in `x`), in the following
#' order:
#' - `corridor` or `riverspace` (only when `corridor` is absent):
#'   drawn first to set the plot extent
#' - `valley`: light grey, transparent fill, no outlines
#' - `segments`: outlined polygons
#' - `corridor`: outlined polygon
#'
#' @param x An object of class [delineation]. This is typically the output
#'   of the [delineate()] function.
#' @param extent The delineation layer whose bounding box sets the plot extent.
#'   One of `"corridor"`, `"valley"`, `"riverspace"`, or `NULL`
#'   (no restriction, ggplot2 fits all layers). Defaults to `"corridor"`.
#' @param legend Whether the plot should display a legend. Default is TRUE.
#'
#' @returns A list of [ggplot2::geom_sf()] and [ggplot2::coord_sf()] elements
#'   to be added to a [ggplot2::ggplot()] object.
#' @seealso [plot.delineation()] for a base R alternative.
#' @export
#'
#' @examplesIf interactive()
#' library(ggplot2)
#' bd <- delineate_city_river("Bucharest", "Dâmbovița")
#' ggplot() + geom_delineation(bd) + theme_void()
#' ggplot() + geom_delineation(bd, extent = "valley") + theme_void()
#'
#' @srrstats {SP2.0b} If object of class other than `delineation` is provided
#'   as input, the function raises an error with an informative message.
#' @srrstats {SP5.0} This function provides a `ggplot2`-based alternative to the
#'   base R `plot()` method.
#' @srrstats {SP5.1} Spatial orientation and aspect ratio are handled by
#'   [ggplot2::coord_sf()], which maps easting to the x-axis and northing to the
#'   y-axis.
#' @srrstats {SP5.2} When axes are displayed, coordinate values are set via
#'   [ggplot2::coord_sf()] to match the units of the CRS used for delineation.
#'   Axis labels are the responsibility of the user's ggplot2 theme and are
#'   conventionally suppressed in cartographic outputs (e.g. with
#'   [ggplot2::theme_void()]).
geom_delineation <- function(x, extent = "corridor", legend = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for geom_delineation(). ",
      "Install it with: install.packages('ggplot2')"
    )
  }
  if (!inherits(x, "delineation")) {
    stop("'x' must be an object of class 'delineation'.")
  }

  x <- unclass(x)

  if (is.null(x$corridor) && is.null(x$segments) && is.null(x$riverspace)) {
    stop("No delineation layers present in the delineation object.")
  }

  if (!is.null(extent)) {
    checkmate::assert_choice(
      extent, c("corridor", "valley", "riverspace"),
    )
    if (is.null(x[[extent]])) {
      warning("Layer '", extent, "' not found; extent will not be restricted.")
      extent <- NULL
    }
  }

  layers <- list()

  # Add layers
  if (!is.null(x$valley)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$valley),
      ggplot2::aes(fill = "Valley"), colour = NA, alpha = 0.2
    )))
  }
  if (!is.null(x$riverspace)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$riverspace),
      fill = "lightblue", colour = NA
    )))
  }
  if (!is.null(x$river_surface)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$river_surface),
      fill = "blue", colour = NA
    )))
  }
  if (!is.null(x$river_centerline)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$river_centerline),
      ggplot2::aes(colour = "River")
    )))
  }
  if (!is.null(x$segments)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$segments),
      fill = NA, linewidth = 0.5
    )))
  }
  if (!is.null(x$corridor)) {
    layers <- c(layers, list(ggplot2::geom_sf(
      data = sf::st_sf(geometry = x$corridor),
      fill = NA, linewidth = 1
    )))
  }

  # Conditionally display legend, visible by default
  guide_val <- if (legend) ggplot2::guide_legend(title = "") else "none"
  layers <- c(layers, list(
    ggplot2::scale_fill_manual(values = c(Valley = "grey50"),
                               guide = guide_val),
    ggplot2::scale_colour_manual(values = c(River = "blue"), guide = guide_val)
  ))

  # Zoom plot to user-specified extent
  if (!is.null(extent)) {
    bb <- sf::st_bbox(x[[extent]])
    layers <- c(layers, list(ggplot2::coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      datum = sf::st_crs(x$aoi$crs)
    )))
  }

  layers
}
