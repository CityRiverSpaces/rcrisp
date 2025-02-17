#' Return the cache directory used by the package
#'
#' By default, the cache directory is equal to `${HOME}/.cache/CRiSp`. A
#' different directory can be used by setting the environment variable
#' `CRISP_CACHE_DIRECTORY`. This can also be done by adding the following
#' line to the `.Renviron` file:
#' `CRISP_CACHE_DIRECTORY=/path/to/crisp/cache/dir`.
#'
#' @return The cache directory used by CRiSp.
#' @export
cache_directory <- function() {
  default_dir <- file.path(Sys.getenv("HOME", "."), ".cache", "CRiSp")
  cache_dir <- Sys.getenv("CRISP_CACHE_DIRECTORY", default_dir)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  normalizePath(cache_dir)
}

#' Get the file path where to cache results of an Overpass API query
#'
#' The function returns the file path where to serialize an osdata_sf object
#' for a given key:value pair and a bounding box. The directory used is the one
#' returned by [`cache_directory()`].
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param bbox A bounding box
#'
#' @return A character string representing the file path
get_osmdata_cache_filepath <- function(key, value, bbox) {
  # collapse `value` (which might be a vector) to a character string
  value_str <- paste(value, collapse = "_")
  # collapse `bbox` components as well, after rounding them
  bbox_str <- bbox_as_str(bbox)

  filename <- get_rds_filename("osmdata", key, value_str, bbox_str)
  file.path(cache_directory(), filename)
}

#' Get file path where to cache digital elevation model (DEM) data
#'
#' The function returns the file path where to serialize a [`terra::SpatRaster`]
#' object representing the DEM as retrieved from a set of tiles reachable at the
#' given URLs, cropped and merged for the given bounding box. The directory used
#' is the one returned by [`cache_directory()`].
#'
#' @param tile_urls URL-paths where to reach the DEM tiles
#' @param bbox A bounding box
#'
#' @return A character string representing the file path
get_dem_cache_filepath <- function(tile_urls, bbox) {
  # concatenate tile names
  tile_names <- sapply(
    tile_urls,
    # get the base name of the tile and strip its extension
    \(x) sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
  )
  tiles_str <- paste(tile_names, collapse = "_")

  # collapse `bbox` components, after rounding them
  bbox_str <- bbox_as_str(bbox)

  filename <- get_rds_filename("dem", tiles_str, bbox_str)
  file.path(cache_directory(), filename)
}

#' @noRd
bbox_as_str <- function(bbox, digits = 3) {
  bbox_rounded <- round(bbox, digits)
  paste(as.character(bbox_rounded), collapse = "_")
}

#' @noRd
get_rds_filename <- function(...) {
  stem <- paste(..., sep = "_")
  paste(stem, "rds", sep = ".")
}

#' Read data from the cache directory
#'
#' For the directory used for caching see [`cache_directory()`].
#'
#' @param filepath Path of the file to deserialize as a character string
#' @param unwrap Whether the deserialized object should be "unpacked" (as
#'   required by [`terra::SpatRaster`] objects)
#' @param quiet Omit warning on cache file being loaded
#'
#' @return Object deserialized
read_data_from_cache <- function(filepath, unwrap = FALSE, quiet = FALSE) {
  if (!quiet) {
    warning(sprintf("Loading data from cache directory: %s", filepath))
  }
  data <- readRDS(filepath)
  if (unwrap) {
    data <- terra::unwrap(data)
  }
  data
}

#' Write data to the cache directory
#'
#' For the directory used for caching see [`cache_directory()`].
#'
#' @param x Object to serialize to a file
#' @param filepath Path where to serialize x, as a character string
#' @param wrap Whether the object should be "packed" before serialization (as
#'   required by [`terra::SpatRaster`] objects)
#' @param quiet Omit warning on cache file being written
#'
#' @return `NULL` invisibly
write_data_to_cache <- function(x, filepath, wrap = FALSE, quiet = FALSE) {
  if (!quiet) {
    warning(sprintf("Saving data to cache directory: %s", filepath))
  }
  if (wrap) {
    x <- terra::wrap(x)
  }
  saveRDS(x, filepath)
}
