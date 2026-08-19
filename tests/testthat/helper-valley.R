#' Gererate an artificial DEM representing a valley.
#'
#' Build the DEM representing an artificial valley around the given river
#' geometry, using a fixed valley slope.
get_test_dem_valley <- function(
  river, resolution = 30, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
  slope = 0.2, min_height = 0., max_height = NULL
) {
  bbox <- sf::st_bbox(river)
  if (is.null(xmin)) xmin <- bbox$xmin
  if (is.null(xmax)) xmax <- bbox$xmax
  if (is.null(ymin)) ymin <- bbox$ymin
  if (is.null(ymax)) ymax <- bbox$ymax
  if (xmin == xmax || ymin == ymax) stop("Invalid extend to build the river!")
  crs <- terra::crs(sf::st_crs(river)$input)
  d <- terra::rast(nlyrs = 1, xmin = xmin, xmax = xmax, ymin = ymin,
                   ymax = ymax, crs = crs, resolution = resolution, vals = 0) |>
    terra::distance(terra::vect(river))
  height <- min_height + slope * d
  if (!is.null(max_height)) {
    terra::mask(
      height, terra::ifel(height > max_height, NA, 1), updatevalue = max_height
    )
  } else {
    height
  }
}
