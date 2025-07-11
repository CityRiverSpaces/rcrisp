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
#'
#' @returns A plot visualizing the delineation object.
#' @export
#'
#' @examplesIf interactive()
#' bd <- delineate("Bucharest", "Dâmbovița")
#' plot(bd)
plot.delineation <- function(x) {
  if (!inherits(x, "delineation")) {
    stop("The object is not of class 'delineation'")
  }

  x <- unclass(x)

  plot(x$corridor)
  if (!is.null(x$valley)) {
    plot(x$valley, col = "yellow", border = NA, add = TRUE)
  }
  if (!is.null(x$riverspace)) {
    plot(x$riverspace, col = "lightblue", border = NA, add = TRUE)
  }
  plot(x$river_centerline, col = "blue", add = TRUE)
  plot(x$railways, col = "darkgrey", add = TRUE, lwd = 0.5)
  plot(x$streets$geometry, add = TRUE)
  if (!is.null(x$segments)) {
    plot(x$segments, border = "orange", add = TRUE, lwd = 3)
  }
  if (!is.null(x$corridor)) {
    plot(x$corridor, border = "red", add = TRUE, lwd = 3)
  }
}
