#' Get example OSM data
#'
#' This function retrieves example OpenStreetMap (OSM) data from the
#' 4TU.ResearchData data repository, and it can be used in examples
#' and tests. The code used to generate the example dataset is available at
#' https://github.com/CityRiverSpaces/CRiSpExampleData. Note that the example
#' dataset is cached locally, so that subsequent calls to the function can
#' load the example data from disk without having to re-download the data.
#'
#' @srrstats {G5.0} The dataset used in examples, tests and vignettes is
#'   retrieved from a persistent location on a data repository. The dataset
#'   follows FAIR data standards and thus its properties are described in
#'   detail.
#' @srrstats {G5.1} The example dataset is made generally available to users
#'   through the `get_osm_example_data()` function which retrieves the data
#'   from a persistent URL. The code used to generate the example dataset is
#'   publicly available, as described in the Roxygen comments.
#'
#' @param force_download Download data even if cached data is available
#' @return A list of sf objects containing the OSM data.
#' @importFrom utils download.file
#' @importFrom stats setNames
#' @export
#'
#' @examplesIf interactive()
#' get_osm_example_data()
get_osm_example_data <- function(force_download = FALSE) {
  file <- get_example_data_file("bucharest_osm.gpkg",
                                force_download = force_download)
  names <- sf::st_layers(file)$name
  lapply(names, \(layer) sf::st_read(file, layer = layer, quiet = TRUE)) |>
    setNames(names)
}

#' Get example DEM data
#'
#' This function retrieves example Digital Elevation Model (DEM) data from a
#' 4TU.ResearchData data repository, and it can be used in examples and tests.
#' The code used to generate the example dataset is available at
#' https://github.com/CityRiverSpaces/CRiSpExampleData. Note that the example
#' dataset is cached locally, so that subsequent calls to the function can
#' load the example data from disk without having to re-download the data.
#'
#' @param force_download Download data even if cached data is available
#' @return A SpatRaster object containing the DEM data.
#' @export
#'
#' @examplesIf interactive()
#' get_dem_example_data()
get_dem_example_data <- function(force_download = FALSE) {
  file <- get_example_data_file("bucharest_dem.tiff",
                                force_download = force_download)
  terra::rast(file)
}

#' Retrieve an example data file from the data repository
#'
#' Store the file in the cache directory, for subsequent reuse.
#'
#' @param force_download Download data even if cached data is available
#' @return A character string representing the file path
#' @keywords internal
get_example_data_file <- function(filename, force_download = FALSE) {
  filepath <- get_example_cache_filepath(filename)

  if (!file.exists(filepath) || force_download) {
    download_urls <- get_download_urls()
    download_url <- download_urls[[filename]]
    stopifnot(!is.null(download_url))
    # temporarily increase timeout, reset value on exit
    op <- options(timeout = 120)
    on.exit(options(op))
    retry(download.file, url = download_url, destfile = filepath, mode = "wb",
          quiet = TRUE)
  }

  filepath
}

#' Parse the metadata retrieved from 4TU.ResearchData to extract
#' download URLs for each of the files in the example data repository
#'
#' @noRd
get_download_urls <- function() {
  resp <- retry(get_body_json, example_metadata_url)
  download_urls <- list()
  for (file in resp$files) {
    download_urls[file$name] <- file$download_url
  }
  download_urls
}

#' Parse the JSON response body from a request to the given URL
#'
#' @noRd
get_body_json <- function(url) {
  httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Retry function call, for API calls and external services
#'
#' @noRd
retry <- function(func, ..., max_retries = 5, delay = 2) {
  attempt <- 1
  while (attempt <= max_retries) {
    result <- tryCatch({
      func(...)  # Call the function with arguments
    }, error = function(e) {
      warning(sprintf("Attempt %d failed: %s", attempt, e$message))
      NULL
    })

    if (!is.null(result)) {
      return(result)  # Successfully retrieved result
    }

    message(sprintf("Retrying in %d seconds...", delay))
    Sys.sleep(delay)
    attempt <- attempt + 1
  }

  stop("Function failed after multiple attempts.")
}

#' Example data files that can be used in examples and tests are stored in
#' a 4TU.ResearchData data repository, reachable at the URL:
#' https://data.4tu.nl/datasets/f5d5e118-b5bd-4dfb-987f-fe10d1b9b .
#' Repository metadata can be accessed programmatically via the API call encoded
#' in the given URL.
#'
#' @noRd
example_metadata_url <- "https://data.4tu.nl/v2/articles/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386"  # nolint
