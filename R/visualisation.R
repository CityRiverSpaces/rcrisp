#' Plot a delineation object
#'
#' This function provides a way to quickly visulise how the layers of a
#' delineation object fit together. A delineation object typically includes the
#' base layers `streets`, `railways`, `river_centerline` and `river_surface`,
#' and the delineations of the `valley`, `corridor`, `segments`, and
#' `riverspace`. Depending on the delineation object, some of the delineation
#' layers may not be present and thus will not be plotted.
#'
#' @param x An object of class `delineation`. This is typically the output
#'   of the `delineate()` function.
#' @param ... Additional arguments passed to the `plot()` function for
#'   the `segments` and `corridor` layers.
#'
#' @returns A plot visualizing the delineation object.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate("Bucharest", "Dâmbovița")
#' plot(bd)
plot.delineation <- function(x, ...) {
  if (!inherits(x, "delineation")) {
    stop("The object is not of class 'delineation'")
  }

  x <- unclass(x)

  # Set plot extent with first layer
  if (!is.null(x$corridor)) {
    plot(x$corridor)
  } else if (!is.null(x$riverspace)) {
    # The only case when corridor may be absent
    plot(x$riverspace, col = "lightblue", border = NA, add = TRUE)
  } else {
    stop("No delineation layers present in the delineation object.")
  }

  base_layers <- c("streets", "railways", "river_centerline")
  if (!all(base_layers %in% names(x))) {
    warning(paste("Not all base layers found in the delineation object.",
                  "Plotting without those."))
  }

  # Plot the layers
  if (!is.null(x$valley)) {
    plot(x$valley, col = "grey80", border = NA, add = TRUE)
  }
  if (!is.null(x$river_surface)) {
    plot(x$river_surface, col = "blue", border = NA, add = TRUE)
  }
  if (!is.null(x$river_centerline)) {
    plot(x$river_centerline, col = "blue", add = TRUE)
  }
  if (!is.null(x$railways)) {
    plot(x$railways$geometry, add = TRUE, lwd = 0.5)
  }
  if (!is.null(x$streets)) {
    plot(x$streets$geometry, add = TRUE)
  }
  if (!is.null(x$segments)) {
    plot(x$segments, ..., add = TRUE, lwd = 2)
  }
  if (!is.null(x$corridor)) {
    plot(x$corridor, ..., add = TRUE, lwd = 3)
  }
}
