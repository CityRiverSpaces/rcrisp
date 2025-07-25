#' Retrieve OpenStreetMap data as sf object
#'
#' Query the Overpass API for a key:value pair within a given bounding box
#' (provided as lat/lon coordiates). Results are cached, so that new queries
#' with the same input parameters will be loaded from disk.
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data
#' @param aoi An area of interest, provided either as as sf object or "bbox" or
#' as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param force_download Download data even if cached data is available
#'
#' @return An [`osmdata::osmdata`] object with the retrieved OpenStreetMap data
#' @export
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' osmdata_as_sf("highway", "motorway", bb, force_download = TRUE)
#' @srrstats {G2.7} The `aoi` parameter accepts domain-specific tabular input
#'   of type `sf`.
#' @srrstats {G4.0} OSM data is saved with a file name concatenated from the
#'   OSM "key", "value" and "bbox" coordinates.
#' @srrstats {G2.3, G2.3b} Both `key` and `value` are made case-insensitive to
#'   comply with OpenStreetMap (OSM) naming convention. Values are not checked
#'   as they depend on the key. Keys and values should be retrieved from OSM
#'   documentation.
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is of class
#'   [`osmdata::osmdata`], explicitly documented as such.
osmdata_as_sf <- function(key, value, aoi, force_download = FALSE) {
  # Check input
  checkmate::assert_character(key, len = 1)
  checkmate::assert_character(value, min.len = 1)
  checkmate::assert_true(inherits(aoi, c("sf", "sfc", "bbox")))
  checkmate::assert_logical(force_download, len = 1)

  bbox <- as_bbox(aoi) # it should be in lat/lon

  key <- tolower(key)
  value <- tolower(value)

  filepath <- get_osmdata_cache_filepath(key, value, bbox)

  if (file.exists(filepath) && !force_download) {
    osmdata_sf <- read_data_from_cache(filepath)
    return(osmdata_sf)
  }

  osmdata_sf <- osmdata_query(key, value, bbox)

  write_data_to_cache(osmdata_sf, filepath)

  osmdata_sf
}

#' Query the Overpass API for a key:value pair within a bounding box
#'
#' @param key A character string with the key to filter the data
#' @param value A character string with the value to filter the data. If
#'  value = "" means that you get all features available in OSM for the
#'  specified bounding box
#' @param bb A bounding box, in lat/lon coordinates
#'
#' @return An [`osmdata::osmdata`] object with the retrieved OpenStreetMap data
#' @keywords internal
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is of class
#'   [`osmdata::osmdata`], explicitly documented as such.
osmdata_query <- function(key, value, bb) {
  # this is needed because the add_osm_feature does not support
  # value as an empty string
  if (all(value == "")) value <- NULL
  bb |>
    osmdata::opq() |>
    osmdata::add_osm_feature(key = key, value = value) |>
    osmdata::osmdata_sf()
}

#' Get the bounding box of a city
#'
#' @param city_name The name of the city
#'
#' @return A `bbox` object with the bounding box of the city
#' @export
#'
#' @examplesIf interactive()
#' get_osm_bb("Bucharest")
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a `bbox` object as
#'   returned by [`sf::st_bbox()`], explicitly documented as such.
get_osm_bb <- function(city_name) {
  bb <- osmdata::getbb(city_name)
  as_bbox(bb)
}

#' Retrieve OpenStreetMap data for a given location
#'
#' Retrieve OpenStreetMap data for a given location, including
#' the city boundary, the river centreline and surface, the streets, the
#' railways, and the buildings
#'
#' @param city_name A character string with the name of the city.
#' @param river_name A character string with the name of the river.
#' @param network_buffer Buffer distance in meters around the river
#'   to get the streets and railways, default is 0 means no
#'   network data will be downloaded
#' @param buildings_buffer Buffer distance in meters around the river
#'   to get the buildings, default is 0 means no
#'   buildings data will be downloaded
#' @param city_boundary A logical indicating if the city boundary should be
#'   retrieved. Default is TRUE.
#' @param crs An integer with the EPSG code for the projection. If no CRS is
#'   specified, the default is the UTM zone for the city.
#' @param force_download Download data even if cached data is available
#'
#' @return A list with the retrieved OpenStreetMap data sets for the
#'         given location, as objects of class [`sf::sfc`]
#' @export
#'
#' @examplesIf interactive()
#' # Set parameters
#' city <- "Bucharest"
#' river <- "Dâmbovița"
#'
#' # Get OSM data with defaults
#' get_osmdata(city, river)
#'
#' # Use custom network buffer
#' get_osmdata(city, river, network_buffer = 3500)
#'
#' # Use custom buildings buffer
#' get_osmdata(city, river, buildings_buffer = 150)
#'
#' # Use custom CRS
#' get_osmdata(city, river, crs = "EPSG:31600")  # National projected CRS
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a list of objects of
#'   class [`sf::sfc`], explicitly documented as such.
get_osmdata <- function(
  city_name, river_name, network_buffer = NULL, buildings_buffer = NULL,
  city_boundary = TRUE, crs = NULL, force_download = FALSE
) {
  # Check input
  checkmate::assert_logical(force_download, len = 1)
  checkmate::assert_logical(city_boundary, len = 1)

  bb <- get_osm_bb(city_name)
  # If not provided, determine the CRS
  if (is.null(crs)) crs <- get_utm_zone(bb)

  # Retrieve the river center line and surface
  river <- get_osm_river(
    bb, river_name, crs = crs, force_download = force_download
  )

  osm_data <- list(
    bb = bb,
    river_centerline = river$centerline,
    river_surface = river$surface
  )

  # Retrieve streets and railways based on the aoi
  if (!is.null(network_buffer)) {
    aoi_network <- get_river_aoi(river, bb, buffer_distance = network_buffer)
    osm_data <- append(osm_data, list(aoi_network = aoi_network))
    osm_data <- append(osm_data, list(
      streets = get_osm_streets(aoi_network, crs = crs,
                                force_download = force_download)
    ))
    osm_data <- append(osm_data, list(
      railways = get_osm_railways(aoi_network, crs = crs,
                                  force_download = force_download)
    ))
  }

  # Retrieve buildings based on a different aoi
  if (!is.null(buildings_buffer)) {
    aoi_buildings <- get_river_aoi(river, bb,
                                   buffer_distance = buildings_buffer)
    osm_data <- append(osm_data, list(aoi_buildings = aoi_buildings))
    osm_data <- c(osm_data, list(
      buildings = get_osm_buildings(aoi_buildings, crs = crs,
                                    force_download = force_download)
    ))
  }

  if (city_boundary) {
    osm_data <- c(osm_data, list(
      boundary = get_osm_city_boundary(bb, city_name, crs = crs,
                                       force_download = force_download)
    ))
  }

  osm_data
}

#' Get the city boundary from OpenStreetMap
#'
#' This function retrieves the city boundary from OpenStreetMap based on a
#' bounding box with the OSM tags "place:city" and "boundary:administrative".
#' The result is filtered by the city name.
#'
#' @param bb Bounding box of class `bbox`
#' @param city_name A character string with the name of the city
#' @param crs Coordinate reference system as EPSG code
#' @param multiple A logical indicating if multiple city boundaries should be
#'                 returned. By default, only the first one is returned.
#' @param force_download Download data even if cached data is available
#'
#' @return An object of class [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`]
#'   with the city boundary
#' @importFrom rlang .data
#' @export
#'
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_city_boundary(bb, "Bucharest", crs)
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a an object of
#'   class [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`], explicitly
#'   documented as such.
get_osm_city_boundary <- function(bb, city_name, crs = NULL, multiple = FALSE,
                                  force_download = FALSE) {
  # Check input
  checkmate::assert_logical(multiple, len = 1)
  checkmate::assert_logical(force_download, len = 1)

  # Drop country if specified after comma
  city_name_clean <- stringr::str_extract(city_name, "^[^,]+")
  # Define a helper function to fetch the city boundary
  fetch_boundary <- function(key, value) {
    osmdata_sf <- osmdata_as_sf(key, value, bb, force_download = force_download)
    dplyr::bind_rows(osmdata_sf$osm_polygons, osmdata_sf$osm_multipolygons) |>
      # filter using any of the "name" columns (matching different languages)
      match_osm_name(city_name_clean) |>
      sf::st_geometry()
  }

  # Try to get the city boundary with the "boundary:administrative" tag
  city_boundary <- tryCatch(fetch_boundary("boundary", "administrative"),
                            error = function(e) NULL)

  if (is.null(city_boundary) || length(city_boundary) == 0) {
    stop("No city boundary found. The city name may be incorrect.")
  }

  crs <- as_crs(crs)
  if (!is.null(crs)) city_boundary <- sf::st_transform(city_boundary, crs)

  if (length(city_boundary) > 1) {
    if (!multiple) {
      message("Multiple boundaries were found. Using the first one.")
      return(city_boundary[1])
    } else {
      message("Multiple boundaries were found. Returning all.")
    }
  }

  city_boundary
}

#' Get the river centreline and surface from OpenStreetMap
#'
#' @param bb Bounding box of class `bbox`
#' @param river_name The name of the river
#' @param crs Coordinate reference system as EPSG code
#' @param force_download Download data even if cached data is available
#'
#' @return A list with the river centreline as object of class
#'   [`sf::sfc_LINESTRING`] or [`sf::sfc_MULTILINESTRING`] and river surface of
#'   class [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`].
#' @export
#'
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_river(bb, "Dâmbovița", crs)
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a list an object of
#'   class [`sf::sfc_LINESTRING`] or [`sf::sfc_MULTILINESTRING`] and an object
#'   of class [`sf::sfc_POLYGON`] or [`sf::sfc_MULTIPOLYGON`], explicitly
#'   documented as such.
get_osm_river <- function(bb, river_name, crs = NULL, force_download = FALSE) {
  # Check input
  checkmate::assert_logical(force_download, len = 1)

  # Get the river centreline
  river_centerline <- osmdata_as_sf("waterway", "", bb,
                                    force_download = force_download)

  # Check that waterway geometries are found within bb
  if (is.null(river_centerline$osm_lines) &&
        is.null(river_centerline$osm_multilines)) {
    stop(sprintf("No waterway geometries found within given bounding box"))
  }

  river_centerline_lines <- river_centerline$osm_lines
  if (!is.null(river_centerline$osm_multilines)) {
    river_centerline_lines <- dplyr::bind_rows(river_centerline_lines,
                                               river_centerline$osm_multilines)
  }

  # Retrieve river centerline of interest
  river_centerline <- river_centerline_lines |>
    # filter using any of the "name" columns (matching different languages)
    match_osm_name(river_name) |>
    check_invalid_geometry() |> # fix invalid geometries, if any
    # the query can return more features than actually intersecting the bb
    sf::st_filter(sf::st_as_sfc(bb), .predicate = sf::st_intersects) |>
    sf::st_geometry() |>
    sf::st_union()

  if (sf::st_is_empty(river_centerline)) stop(
    sprintf("No river geometry found for %s", river_name)
  )

  # Get the river surface
  river_surface <- osmdata_as_sf("natural", "water", bb,
                                 force_download = force_download)
  river_surface_polygons <- river_surface$osm_polygons
  if (!is.null(river_surface$osm_multipolygons)) {
    river_surface_polygons <- dplyr::bind_rows(river_surface_polygons,
                                               river_surface$osm_multipolygons)
  }

  river_surface <- river_surface_polygons |>
    sf::st_geometry() |>
    check_invalid_geometry() |> # fix invalid geometries, if any
    sf::st_as_sf() |>
    sf::st_filter(river_centerline, .predicate = sf::st_intersects) |>
    sf::st_union()

  if (!is.null(crs)) {
    crs <- as_crs(crs)
    river_centerline <- sf::st_transform(river_centerline, crs)
    river_surface <- sf::st_transform(river_surface, crs)
  }

  list(centerline = river_centerline, surface = river_surface)
}

#' Get OpenStreetMap streets
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs A numeric vector of length one with the EPSG code of the CRS
#' @param highway_values A character vector with the highway values to retrieve.
#'             If left NULL, the function retrieves the following values:
#'             "motorway", "trunk", "primary", "secondary", "tertiary"
#' @param force_download Download data even if cached data is available
#'
#' @return An object of class [`sf::sfc_LINESTRING`]
#' @export
#' @importFrom rlang !! sym
#'
#' @examplesIf interactive()
#' # Set parameters
#' bb <- get_osm_bb("Bucharest")
#' crs <- 31600  # National projected CRS
#'
#' # Get streets with default values
#' get_osm_streets(bb, crs)
#'
#' # Specify street categories to be retrieved
#' get_osm_streets(bb, crs, highway_values = "primary")
#'
#' # Ensure that data is not retrieved from cache
#' get_osm_streets(bb, crs, force_download = TRUE)
#' @srrstats {G2.13} The absence of missing values in numeric inputs is
#'   asserted using the `checkmate` package.
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a an object of
#'   class [`sf::sfc_LINESTRING`], explicitly documented as such.
get_osm_streets <- function(aoi, crs = NULL, highway_values = NULL,
                            force_download = FALSE) {
  # Check input
  checkmate::assert_true(inherits(aoi, c("sf", "sfc", "bbox")))
  checkmate::assert_numeric(crs,
                            null.ok = TRUE,
                            any.missing = FALSE)
  checkmate::assert_character(highway_values, null.ok = TRUE)
  checkmate::assert_logical(force_download, len = 1)

  if (is.null(highway_values)) {
    highway_values <- c("motorway", "trunk", "primary", "secondary", "tertiary")
    link_values <- vapply(X = highway_values,
                          FUN = \(x) sprintf("%s_link", x),
                          FUN.VALUE = character(1),
                          USE.NAMES = FALSE)
    highway_values <- c(highway_values, link_values)
  }

  streets <- osmdata_as_sf("highway", highway_values, aoi,
                           force_download = force_download)

  # Cast polygons (closed streets) into lines
  poly_to_lines <- suppressWarnings(
    streets$osm_polygons |> sf::st_cast("LINESTRING")
  )

  # Combine all features in one data frame
  streets_lines <- streets$osm_lines |>
    dplyr::bind_rows(poly_to_lines) |>
    dplyr::select("highway") |>
    dplyr::rename(!!sym("type") := !!sym("highway"))

  # Intersect with the bounding polygon
  # this will return a warning, see https://github.com/r-spatial/sf/issues/406
  if (inherits(aoi, "bbox")) aoi <- sf::st_as_sfc(aoi)
  mask <- sf::st_intersects(streets_lines, aoi, sparse = FALSE)
  streets_lines <- streets_lines[mask, ]

  if (!is.null(crs)) {
    crs <- as_crs(crs)
    streets_lines <- sf::st_transform(streets_lines, crs)
  }

  streets_lines
}

#' Get OpenStreetMap railways
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs A numeric vector of length one with the EPSG code of the CRS
#' @param railway_values A character or character vector with the railway values
#'   to retrieve.
#' @param force_download Download data even if cached data is available
#'
#' @return An object of class [`sf::sfc_LINESTRING`]
#' @export
#' @importFrom rlang !! sym
#'
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_railways(bb, crs)
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a an object of
#'   class [`sf::sfc_LINESTRING`], explicitly documented as such.
get_osm_railways <- function(aoi, crs = NULL, railway_values = "rail",
                             force_download = FALSE) {
  # Check input
  checkmate::assert_character(railway_values, min.len = 1)
  checkmate::assert_logical(force_download, len = 1)

  railway_values <- tolower(railway_values)
  railways <- osmdata_as_sf("railway", railway_values, aoi,
                            force_download = force_download)
  # If no railways are found, return an empty sf object
  if (is.null(railways$osm_lines)) {
    if (is.null(crs)) crs <- as_crs("EPSG:4326", allow_geographic = TRUE)
    empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = crs))
    return(empty_sf)
  }

  railways_lines <- railways$osm_lines |>
    dplyr::select("railway") |>
    dplyr::rename(!!sym("type") := !!sym("railway"))

  # Intersect with the bounding polygon
  if (inherits(aoi, "bbox")) aoi <- sf::st_as_sfc(aoi)
  mask <- sf::st_intersects(railways_lines, aoi, sparse = FALSE)
  railways_lines <- railways_lines[mask, ]

  if (!is.null(crs)) {
    crs <- as_crs(crs)
    railways_lines <- sf::st_transform(railways_lines, crs)
  }

  railways_lines
}

#' Get OpenStreetMap buildings
#'
#' Get buildings from OpenStreetMap within a given buffer around a river.
#'
#' @param aoi Area of interest as sf object or bbox
#' @param crs Coordinate reference system as EPSG code
#' @param force_download Download data even if cached data is available
#'
#' @return An object of class [`sf::sfc_POLYGON`]
#' @export
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' crs <- get_utm_zone(bb)
#' get_osm_buildings(bb, crs)
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a an object of
#'   class [`sf::sfc_POLYGON`], explicitly documented as such.
get_osm_buildings <- function(aoi, crs = NULL, force_download = FALSE) {
  # Check input
  checkmate::assert_logical(force_download, len = 1)

  buildings <- osmdata_as_sf("building", "", aoi,
                             force_download = force_download)
  buildings <- buildings$osm_polygons |>
    check_invalid_geometry() |> # fix invalid geometries, if any
    sf::st_filter(aoi, .predicate = sf::st_intersects) |>
    dplyr::filter(.data$building != "NULL") |>
    sf::st_geometry()

  if (!is.null(crs)) {
    crs <- as_crs(crs)
    buildings <- sf::st_transform(buildings, crs)
  }

  buildings
}

#' Get an area of interest (AoI) around a river, cropping to the bounding box of
#' a city
#'
#' @param river A list with the river centreline and surface geometries
#' @param city_bbox Bounding box of class `bbox` around the city
#' @param buffer_distance Buffer size around the river
#' @return An [`sf::sfc_POLYGON`] object in lat/lon coordinates
#' @export
#'
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' river <- get_osm_river(bb, "Dâmbovița")
#' get_river_aoi(river, bb, buffer_distance = 100)
#' @srrstats {G2.7} The `river` parameter accepts domain-specific tabular input
#'   of type `sf`.
#' @srrstats {G2.13} The absence of missing values in numeric inputs is
#'   asserted using the `checkmate` package.
#' @srrstats {SP4.0, SP4.0b, SP4.2} The return value is a an object of
#'   class [`sf::sfc_POLYGON`], explicitly documented as such. The returned area
#'   of interest is in geographic CRS as it is meant to be used for clipping
#'   OpenStreetMap data.
get_river_aoi <- function(river, city_bbox, buffer_distance) {
  # Check input
  checkmate::assert_numeric(buffer_distance,
                            len = 1,
                            any.missing = FALSE)

  river <- c(river$centerline, river$surface)

  # Make sure crs are the same for cropping with bb
  river <- sf::st_transform(river, sf::st_crs(city_bbox))

  river_buffer(river, buffer_distance, bbox = city_bbox)
}

#' Match OpenStreetMap data by name
#'
#' @param osm_data An sf object with OpenStreetMap data
#' @param match A character string with the name to match
#'
#' @return sf object containing only rows with filtered name
#' @keywords internal
match_osm_name <- function(osm_data, match) {
  # Function to find partial matches across rows of a data frame
  includes_match <- \(x) grepl(match, x, ignore.case = TRUE)
  # Apply function above to all columns whose name starts with "name", thus
  # checking for matches in all listed languages
  dplyr::filter(osm_data, dplyr::if_any(dplyr::matches("name"), includes_match))
}
