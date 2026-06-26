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
