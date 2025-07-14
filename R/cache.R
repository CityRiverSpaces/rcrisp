#' Return the cache directory used by the package
#'
#' By default, the user-specific directory as returned by
#' [`tools::R_user_dir()`] is used. A different directory can be used by
#' setting the environment variable `CRISP_CACHE_DIRECTORY`. This can also be
#' done by adding the following line to the `.Renviron` file:
#' `CRISP_CACHE_DIRECTORY=/path/to/crisp/cache/dir`.
#'
#' @return The cache directory used by rcrisp.
#' @export
#' @examplesIf interactive()
#' cache_directory()
cache_directory <- function() {
  default_dir <- tools::R_user_dir("rcrisp", which = "cache")
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
#' @keywords internal
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
#' @keywords internal
get_dem_cache_filepath <- function(tile_urls, bbox) {
  # concatenate tile names
  tile_names <- vapply(
    X = tile_urls,
    # get the base name of the tile and strip its extension
    FUN = \(x) sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x)),
    FUN.VALUE = character(1)
  )
  tiles_str <- paste(tile_names, collapse = "_")

  # collapse `bbox` components, after rounding them
  bbox_str <- bbox_as_str(bbox)

  filename <- get_rds_filename("dem", tiles_str, bbox_str)
  file.path(cache_directory(), filename)
}

#' Convert a bounding box to a string
#'
#' @noRd
bbox_as_str <- function(bbox, digits = 3) {
  bbox_rounded <- round(bbox, digits)
  paste(as.character(bbox_rounded), collapse = "_")
}

#' Get RDS filename
#'
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
#' @keywords internal
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
#' Write object in a serialised form (RDS) to a cache directory. For the
#' directory used for caching see [`cache_directory()`].
#'
#' @param x Object to serialize to a file
#' @param filepath Path where to serialize x, as a character string
#' @param wrap Whether the object should be "packed" before serialization (as
#'   required by [`terra::SpatRaster`] objects)
#' @param quiet Omit message on cache file being written
#'
#' @return `NULL` invisibly
#' @keywords internal
write_data_to_cache <- function(x, filepath, wrap = FALSE, quiet = FALSE) {
  if (!quiet) {
    message(sprintf("Saving data to cache directory: %s", filepath))
  }
  if (wrap) {
    x <- terra::wrap(x)
  }
  saveRDS(x, filepath)
}

#' Remove cache files
#'
#' Remove files from cache directory either before a given date or entirely.
#'
#' @param before_date Date before which cache files should be removed provided
#'   as Date
#'
#' @return List of file paths of removed files
#' @export
#' @examplesIf interactive()
#' clear_cache()
clear_cache <- function(before_date = NULL) {
  # Check input
  assert_date(before_date, null.ok = TRUE)

  cache_dir <- cache_directory()
  files <- list.files(cache_dir, full.names = TRUE)

  if (!is.null(before_date)) {
    file_info <- file.info(files)
    files_before_date <- files[file_info$mtime < as.POSIXct(before_date)]
    file.remove(files_before_date)
    files_remaining <- list.files(cache_dir, full.names = TRUE)
    if (all(!files_before_date %in% files_remaining)) {
      message("Cache files successfully removed.")
    }
  } else {
    file.remove(files)
    if (length(list.files(cache_dir)) == 0) {
      message("Cache files successfully removed.")
    }
  }
}

#' Check cache
#'
#' A warning is raised if the cache size is > 100 MB or if it includes files
#' older than 30 days.
#'
#' @export
#' @return `NULL`
#' @examplesIf interactive()
#' check_cache()
check_cache <- function() {
  cache_dir <- cache_directory()
  files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  file_info <- file.info(files)
  cache_size <- sum(file_info$size) / 2 ** 20 # to MB
  is_too_big <- cache_size > 100
  date_oldest_file <- sort(file_info$mtime)[1]
  if (is.na(date_oldest_file)) {
    is_too_old <- FALSE
  } else {
    is_too_old <- (Sys.time() - date_oldest_file) > "30 days"
  }
  if (is_too_big || is_too_old) {
    warning(sprintf(paste0(
      "Cache dir: %s - size: %.0f MB - oldest file from: %s.\n",
      "Clean up files older than 30 days with: `rcrisp::clear_cache('%s')` ",
      "(or remove all cached files with: `rcrisp::clear_cache()`."
    ), cache_dir, cache_size, as.Date(date_oldest_file), Sys.Date() - 30))
  }
}
