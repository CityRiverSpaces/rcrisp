#' Set the units of x as the units of y
#'
#' @param x x (can be unitless)
#' @param y y (can be unitless)
#' @return Object x with units of y
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of the same class and
#'   units as input geometry.
set_units_like <- function(x, y) {
  has_units_x <- inherits(x, "units")
  has_units_y <- inherits(y, "units")
  if ((!has_units_x) && (!has_units_y)) {
    x
  } else if (has_units_y) {
    units::set_units(x, units(y), mode = "standard")
  } else {
    units::drop_units(x)
  }
}

#' Get the UTM zone of a spatial object
#'
#' @param x Object in any class accepted by [`as_bbox()`]
#' @return The EPSG code of the UTM zone
#' @export
#' @examples
#' # Get EPSG code for UTM zone of Bucharest
#' bb <- get_osm_bb("Bucharest")
#' get_utm_zone(bb)
#' @srrstats {SP2.8, SP2.9} Before determining the UTM zone, the bounding box
#'   given as input is transformed into an object of class `bbox`. If input
#'   data does not have a CRS, EPSG:4326 (WGS84) is assumed and assigned by
#'   [`as_bbox()`].
get_utm_zone <- function(x) {
  bb <- as_bbox(x)

  # Make sure the bbox is in lat/lon
  bb <- sf::st_transform(bb, "EPSG:4326")

  if (bb[["ymin"]] < -80 || bb[["ymax"]] > 84) {
    stop("The bbox is outside the UTM validity range (80 deg S; 84 deg N)")
  }
  centroid_long <- (bb[["xmin"]] + bb[["xmax"]]) / 2
  centroid_lat <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  base <- if (centroid_lat >= 0) 32600 else 32700
  base + floor((centroid_long + 180) / 6) + 1
}

#' Get the bounding box from the x object
#'
#' If the x does not have a CRS, WGS84 is assumed.
#'
#' @param x Simple feature object (or compatible) or a bounding box, provided
#'   either as a matrix (with x, y as rows and min, max as columns) or as a
#'   vector (xmin, ymin, xmax, ymax)
#' @return A `bbox` object as returned by [`sf::st_bbox()`]
#' @export
#' @examples
#' library(sf)
#' bounding_coords <- c(25.9, 44.3, 26.2, 44.5)
#' bb <- as_bbox(bounding_coords)
#' class(bb)
#' st_crs(bb)
#' @srrstats {G2.7} The `x` parameter accepts `matrix` or domain-specific
#'   tabular input of type `sf`.
#' @srrstats {G2.8} This function ensures all supported input types are in a
#'   consistent class accepted by `sf::st_bbox()`. Input of class `numeric` and
#'   `matrix`, in particular, are converted to a vector with named elements
#'   (`xmin`, `ymin`, `xmax`, `ymax`) before being passed down to
#'   `sf::st_bbox()`.
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is a `bbox` object as
#'   returned by [`sf::st_bbox()`], explicitly documented as such, and it
#'   maintains the same units as the input if CRS information is available
#'   in the input object.
as_bbox <- function(x) {
  # Check input
  checkmate::assert_true(
    inherits(x, c("sf", "sfc", "numeric", "matrix", "bbox"))
  )

  if (inherits(x, c("numeric", "matrix"))) {
    x <- as.vector(x)
    names(x) <- c("xmin", "ymin", "xmax", "ymax")
  }
  bbox <- sf::st_bbox(x)
  crs <- sf::st_crs(bbox)
  if (is.na(crs)) sf::st_crs(bbox) <- sf::st_crs("EPSG:4326")
  bbox
}

#' Standardise the coordinate reference system (CRS) of an object
#'
#' @param x An object of class `sf`, `sfc`, `bbox`, or a numeric  or character
#'   vector representing a CRS (e.g., EPSG code).
#'
#' @returns An object of class [sf::crs] with a valid CRS.
#' @export
#'
#' @examples
#' library(sf)
#'
#' # Standardise a numeric EPSG code
#' as_crs(4326, allow_geographic = TRUE)
#'
#' # Standardise a character EPSG code
#' as_crs("EPSG:4326", allow_geographic = TRUE)
#'
#' # Standardise a bbox object
#' bb <- st_bbox(c(xmin = 25.9, ymin = 44.3, xmax = 26.2, ymax = 44.5),
#'                 crs = 4326)
#' as_crs(bb, allow_geographic = TRUE)
#'
#' # Standardise a simple feature object
#' bb_sfc <- st_as_sfc(bb)
#' bb_sf <- st_as_sf(bb_sf)
#' as_crs(bb_sf, allow_geographic = TRUE)
#' as_crs(bb_sfc, allow_geographic = TRUE)
as_crs <- function(x, allow_geographic = FALSE) {
  checkmate::assert_multi_class(x, c("numeric",
                                     "character",
                                     "bbox",
                                     "sf",
                                     "sfc"))
  checkmate::assert_logical(allow_geographic, len = 1)

  crs <- sf::st_crs(x)
  if (!allow_geographic && crs$IsGeographic) {
    stop(paste("The input CRS is geographic (lat/lon),",
               "please provide a projected CRS."))
  }
  crs
}

#' Apply a buffer region to a sf object
#'
#' If the input object is in lat/lon coordinates, the buffer is approximately
#' applied by first transforming the object in a suitable projected CRS,
#' expanding it with the given buffer, and then transforming
#' it back to the lat/lon system.
#'
#' @param obj A sf object
#' @param buffer_distance Buffer distance in meters
#' @param ... Optional parameters passed on to [`sf::st_buffer()`]
#' @return An object of class [`sf::sfc_POLYGON`]
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of the class
#'   [`sf::sfc_POLYGON`], explicitly documented as such, and it maintains the
#'   same units as the input.
buffer <- function(obj, buffer_distance, ...) {
  is_obj_longlat <- sf::st_is_longlat(obj)
  dst_crs <- sf::st_crs(obj)
  # check if obj is a bbox
  is_obj_bbox <- inherits(obj, "bbox")
  if (is_obj_bbox) obj <- sf::st_as_sfc(obj)
  if (!is.na(is_obj_longlat) && is_obj_longlat) {
    crs_meters <- get_utm_zone(obj)
    obj <- sf::st_transform(obj, crs_meters)
  }
  expanded_obj <- sf::st_buffer(obj, buffer_distance, ...)
  if (!is.na(is_obj_longlat) && is_obj_longlat) {
    expanded_obj <- sf::st_transform(expanded_obj, dst_crs)
  }
  if (is_obj_bbox) expanded_obj <- sf::st_bbox(expanded_obj)
  expanded_obj
}

#' Draw a corridor as a fixed buffer region around a river.
#'
#' The river geometry may consist of multiple spatial features, these are
#' optionally cropped using the area of interest, then merged after applying the
#' buffer.
#'
#' @param river A simple feature geometry representing the river
#' @param buffer_distance Size of the buffer (in the river's CRS units)
#' @param bbox Bounding box defining the extent of the area of interest
#' @param side Whether to generate a single-sided buffer with a "flat" end.
#'   This is only applicable if `river` is a (multi)linestring geometry.
#'   Choose between `NULL` (double-sided), `"right"` and `"left"`
#'
#' @return An object of class [`sf::sfc_POLYGON`]
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of the class
#'   [`sf::sfc_POLYGON`], explicitly documented as such, and it maintains the
#'   same units as the input.
river_buffer <- function(river, buffer_distance, bbox = NULL, side = NULL) {
  if (!is.null(bbox)) river <- sf::st_crop(river, bbox)
  if (is.null(side)) {
    river_buf <- buffer(river, buffer_distance)
    return(sf::st_union(river_buf))
  } else {
    if (side == "left") {
      river_buf <- buffer(river, buffer_distance, singleSide = TRUE)
    } else if (side == "right") {
      river_buf <- buffer(river, -buffer_distance, singleSide = TRUE)
    } else {
      stop("If specified, 'side' should be either 'right' or 'left'")
    }
    # Merge all components, than make sure we do not spill over the river by
    # splitting the computed geometry with the river centerline and by
    # selecting the largest region
    splits <- split_by(sf::st_union(river_buf), river)
    river_buf <- splits[find_largest(splits)]
    # Finally drop any eventual hole
    return(sfheaders::sf_remove_holes(river_buf))
  }
}

#' Reproject a raster or vector dataset to the specified
#' coordinate reference system (CRS)
#'
#' @param x Raster (`SpatRaster`) or vector (`sf`) object
#' @param crs CRS to be projected to
#' @param ... Optional arguments for raster or vector reproject functions
#'
#' @return [`sf::sf`], [`sf::sfc`], or [`terra::SpatRaster`] object reprojected
#'   to specified CRS
#' @export
#' @examples
#' # Reproject a raster to EPSG:4326
#' r <- terra::rast(matrix(1:12, nrow = 3, ncol = 4), crs = "EPSG:32633")
#' reproject(r, 4326)
#' @srrstats {G2.7} The `x` parameter also accepts domain-specific tabular
#'   input of type `sf`.
#' @srrstats {SP4.0, SP4.0b, SP4.1} The return value is of class [`sf::sf`],
#'   [`sf::sfc`] or [`terra::SpatRaster`], explicitly documented as such, with
#'   transformed CRS as specified by the `crs` parameter.
reproject <- function(x, crs, ...) {
  if (inherits(x, "SpatRaster")) {
    if (inherits(crs, c("integer", "numeric"))) {
      # terra::crs does not support a numeric value as CRS, convert to character
      crs <- sprintf("EPSG:%s", crs)
    } else if (inherits(crs, "crs")) {
      # terra::crs also does not understand sf::crs objects
      crs <- sprintf("EPSG:%s", crs$epsg)
    }
    terra::project(x, crs, ...)
  } else if (inherits(x, c("bbox", "sfc", "sf"))) {
    sf::st_transform(x, crs, ...)
  } else {
    stop(sprintf("Cannot reproject object type: %s", class(x)))
  }
}

#' Load raster data from one or multiple (remote) files
#'
#' If a bounding box is provided, the file(s) are cropped for the given extent.
#' The resulting rasters are then merged using [`terra::merge`].
#'
#' @param urlpaths Path or URL to the raster file(s)
#' @param bbox A bounding box
#'
#' @return Raster data as a [`terra::SpatRaster`] object
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b} The return value is of class [`terra::SpatRaster`],
#'   explicitly documented as such.
load_raster <- function(urlpaths, bbox = NULL) {
  rasters <- lapply(urlpaths, terra::rast)
  if (!is.null(bbox)) {
    # snap spatial extent outward to include pixels crossed by the boundary
    rasters <- lapply(rasters, terra::crop, terra::ext(bbox), snap = "out")
  }
  if (length(rasters) > 1) {
    do.call(terra::merge, args = rasters)
  } else {
    rasters[[1]]
  }
}

#' Combine river centerline and surface
#'
#' @param river_centerline River line as [`sf::sfc_LINESTRING`] or
#'   [`sf::sfc_MULTILINESTRING`]
#' @param river_surface River surface as [`sf::sfc_POLYGON`] or
#'   [`sf::sfc_MULTIPOLYGON`]
#'
#' @return Combined river as [`sf::sfc_MULTILINESTRING`]
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.1, SP4.2} The return value is of class
#'   [`sf::sfc_MULTILINESTRING`], explicitly documented as such, and it
#'   maintains the same units as the input.
combine_river_features <- function(river_centerline, river_surface) {
  if (is.null(river_surface)) {
    warning("Calculating viewpoints along river centerline.")
    return(river_centerline)
  }
  message("Calculating viewpoints from both river edge and river centerline.")
  river_centerline_clipped <- sf::st_geometry(river_centerline) |>
    sf::st_difference(river_surface)
  c(river_centerline_clipped, sf::st_geometry(river_surface)) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_union()
}

#' Check and fix invalid geometries
#'
#' @param sf_obj An object of class [`sf::sf`] or [`sf::sfc`]
#'
#' @return [`sf::sf`] or [`sf::sfc`] object with valid geometries
#' @keywords internal
#' @srrstats {SP4.0, SP4.0a, SP4.1, SP4.2} The return value is of class
#'   [`sf::sf`] or [`sf::sfc`], with the same units and class as the input,
#'   explicitly documented as such.
check_invalid_geometry <- function(sf_obj) {
  if (!all(sf::st_is_valid(sf_obj))) {
    message("Invalid geometries detected! Fixing them...")
  }
  sf::st_make_valid(sf_obj) # if input valid, it remains unchanged
}
