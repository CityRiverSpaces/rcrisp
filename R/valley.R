#' Default STAC collection
#'
#' Endpoint and collection ID of the default STAC collection where to access
#' digital elevation model (DEM) data. This is the global Copernicus DEM 30
#' dataset hosted on AWS, as listed in the EarthSearch STAC API endpoint.
#' Note that AWS credentials need to be set up in order to access the data (not
#' the catalog).
# nolint start
#' References:
#'  - [EarthSearch STAC API](https://element84.com/earth-search/)
#'  - [Copernicus DEM](https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM)
#'  - [AWS Copernicus DEM datasets](https://copernicus-dem-30m.s3.amazonaws.com/readme.html)
#'  - [Data license](https://docs.sentinel-hub.com/api/latest/static/files/data/dem/resources/license/License-COPDEM-30.pdf)
# nolint end
default_stac_dem <- list(
  endpoint = "https://earth-search.aws.element84.com/v1",
  collection = "cop-dem-glo-30"
)

#' Access digital elevation model (DEM) for a given region
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax"),
#'   in lat/lon coordinates (WGS84 coordinate reference system) of class `bbox`
#' @param dem_source Source of the DEM:
#'   - If "STAC" (default), DEM tiles are searched on a SpatioTemporal Asset
#'     Catalog (STAC) end point, then accessed and mosaicked to the area of
#'     interest
#' @param stac_endpoint URL of the STAC API endpoint (only used if `dem_source`
#'   is `"STAC"`). For more info, see [`get_stac_asset_urls()`]
#' @param stac_collection Identifier of the STAC collection to be queried (only
#'   used if `dem_source` is `"STAC"`). For more info, see
#'   [`get_stac_asset_urls()`]
#' @param crs Coordinate reference system (CRS) which to transform the DEM to
#' @param force_download Download data even if cached data is available
#'
#' @return DEM as a terra `SpatRaster` object
#' @export
#' @examplesIf interactive()
#' # Get DEM with default values
#' bb <- get_osm_bb("Bucharest")
#' crs <- 31600  # National projected CRS
#'
#' # Get DEM with default values
#' get_dem(bb)
#'
#' # Get DEM from custom STAC endpoint
#' get_dem(bb,
#'         stac_endpoint = "some endpoint",
#'         stac_collection = "some collection")
#'
#' # Specify CRS
#' get_dem(bb, crs = crs)
#' @srrstats {G2.3, G2.3b} The input character value for `dem_source` is
#'   converted to uppercase using toupper(), making the check case-insensitive.
#'   A validation is then performed to ensure the value is allowed.
#' @srrstats {G2.7} The `bb` parameter accepts tabular input of class `matrix`.
#' @srrstats {SP6.1} If specified by the user, the CRS is standardised with
#'   `as_crs()` before being used to reproject the DEM.
get_dem <- function(bb, dem_source = "STAC", stac_endpoint = NULL,
                    stac_collection = NULL, crs = NULL,
                    force_download = FALSE) {
  # Check input
  bbox <- as_bbox(bb)
  checkmate::assert_choice(dem_source, "STAC")
  checkmate::assert_character(stac_endpoint, null.ok = TRUE, len = 1)
  checkmate::assert_character(stac_collection, null.ok = TRUE, len = 1)
  crs <- as_crs(crs)
  checkmate::assert_logical(force_download, len = 1)
  dem_source <- toupper(dem_source)
  checkmate::assert_choice(dem_source, c("STAC"))

  if (dem_source == "STAC") {
    asset_urls <- get_stac_asset_urls(bbox, endpoint = stac_endpoint,
                                      collection = stac_collection)
    dem <- load_dem(bbox, asset_urls, force_download = force_download)
  } else {
    stop(sprintf("DEM source %s unknown", dem_source))
  }
  if (!is.null(crs)) {
    dem <- reproject(dem, crs)
  }
  dem
}

#' Extract the river valley from the DEM
#'
#' The slope of the digital elevation model (DEM) is used as friction (cost)
#' surface to compute the cost distance from any grid cell of the raster to
#' the river. A characteristic value (default: the mean) of the cost distance
#' distribution in a region surrounding the river (default: a buffer region of
#' 2 km) is then calculated, and used to threshold the cost-distance surface.
#' The resulting area is then "polygonized" to obtain the valley boundary as a
#' simple feature geometry.
#'
#' @srrstats {G1.3} The Cost Distance algorithm is explained here.
#'
#' @param dem `SpatRaster` object with the digital elevation model of the region
#' @param river An object of class [`sf::sf`] or [`sf::sfc`]
#'   representing the river
#'
#' @return River valley as a simple feature geometry of class `sfc_MULTIPOLYGON`
#' @export
#' @examplesIf interactive()
#' bucharest_osm <- get_osm_example_data()
#' bucharest_dem <- get_dem_example_data()
#' delineate_valley(bucharest_dem, bucharest_osm$river_centerline)
#' @srrstats {G2.7} The `river` parameter accepts domain-specific tabular input
#'   of type `sf`.
delineate_valley <- function(dem, river) {
  # Check input
  checkmate::assert_class(dem, "SpatRaster")
  checkmate::assert_multi_class(river, c("sf", "sfc"))

  if (!terra::same.crs(dem, sf::st_crs(river)$wkt)) {
    stop("DEM and river geometry should be in the same CRS")
  }
  cd_masked <- smooth_dem(dem) |>
    get_slope() |>
    get_cost_distance(river) |>
    mask_cost_distance(river)

  cd_thresh <- get_cd_char(cd_masked)

  get_valley_polygon(cd_masked < cd_thresh)
}

#' Retrieve the URLs of all the assets intersecting a bbox from a STAC API
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax"),
#'   in lat/lon coordinates (WGS84 coordinate referece system) of class `bbox`
#' @param endpoint URL of the STAC API endpoint. To be provided together with
#'   `stac_collection`, or leave blank to use defaults (see
#'   [`default_stac_dem`])
#' @param collection Identifier of the STAC collection to be queried. To be
#'   provided together with `stac_endpoint`, or leave blank to use defaults
#'   (see [`default_stac_dem`])
#'
#' @return A list of URLs for the assets in the collection overlapping with
#'   the specified bounding box
#' @export
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' get_stac_asset_urls(bb)
#'
#' # Use non-default STAC API
#' get_stac_asset_urls(bb,
#'                     endpoint = "some endpoint",
#'                     collection = "some collection")
#' @srrstats {G2.7} The `bb` parameter accepts tabular input of class `matrix`.
get_stac_asset_urls <- function(bb, endpoint = NULL, collection = NULL) {
  # Check input
  bbox <- as_bbox(bb)
  checkmate::assert_character(endpoint, len = 1, null.ok = TRUE)
  checkmate::assert_character(collection, len = 1, null.ok = TRUE)

  if (is.null(endpoint) && is.null(collection)) {
    endpoint <- default_stac_dem$endpoint
    collection <- default_stac_dem$collection
    # check if there is AWS credentials file, otherwise use unsigned requests
    aws_credentials_path <- file.path(Sys.getenv("HOME"), ".aws", "credentials")
    if (!file.exists(aws_credentials_path)) {
      Sys.setenv("AWS_NO_SIGN_REQUEST" = "YES")
    }
  } else if (is.null(endpoint) || is.null(collection)) {
    stop("Provide both or neither of STAC endpoint and collection")
  }

  rstac::stac(endpoint) |>
    rstac::stac_search(collections = collection, bbox = bbox) |>
    rstac::get_request() |>
    rstac::assets_url()
}

#' Retrieve DEM data from a list of STAC assets
#'
#' Load DEM data from a list of tiles, crop and merge using a given bounding
#' box to create a raster DEM for the specified region. Results are cached, so
#' that new queries with the same input parameters will be loaded from disk.
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#'   of class `bbox`.
#' @param tile_urls A list of tiles where to read the DEM data from
#' @param force_download Download data even if cached data is available
#'
#' @return A DEM of class [`terra::SpatRaster`], retrieved and retiled to the
#'   given bounding box
#' @export
#' @examplesIf interactive()
#' bb <- get_osm_bb("Bucharest")
#' tile_urls <- get_stac_asset_urls(bb)
#' load_dem(bb = bb, tile_urls = tile_urls, force_download = TRUE)
#' @srrstats {G4.0} DEM data is written to cache with a file name concatenated
#'   from tile names and boundig box coordinates.
#' @srrstats {G2.7} The `bb` parameter accepts tabular input of class `matrix`.
load_dem <- function(bb, tile_urls, force_download = FALSE) {
  # Check input
  bbox <- as_bbox(bb)
  checkmate::assert_character(tile_urls, min.len = 1)
  checkmate::assert_logical(force_download, len = 1)

  filepath <- get_dem_cache_filepath(tile_urls, bbox)

  if (file.exists(filepath) && !force_download) {
    dem <- read_data_from_cache(filepath, unwrap = TRUE)
    return(dem)
  }

  dem <- load_raster(tile_urls, bbox = bbox)

  write_data_to_cache(dem, filepath, wrap = TRUE)

  dem
}

#' Spatially smooth dem by (window) filtering
#'
#' @param dem raster data of dem
#' @param method smoothing function to be used, e.g. "median",
#'   as accepted by [terra::focal()]
#' @param window size of smoothing kernel
#'
#' @return filtered dem
#' @keywords internal
#' @srrstats {SP3.0, SP3.0a} This function allows `window` (i.e., the size of
#'   the neighourhood) to be set and passed down to the `w` parameter of
#'   `terra::focal()`. With a single value (default), the function employs the
#'   square ("queen") neighbourhood form. With a weight matrix, also accepted
#'   by `terra::focal()`, in which cells on ortogonal direction are assigned
#'   `1` and cells on diagonal direction are assigned `0`, a "rook"
#'   neighbourhood form can also be obtained.
smooth_dem <- function(dem, method = "median", window = 5) {
  dem_smoothed <- terra::focal(dem, w = window, fun = method)
  names(dem_smoothed) <- "dem_smoothed"
  dem_smoothed
}

#' Derive slope as percentage from DEM
#'
#' This makes use of the terrain function of the terra package
#'
#' @param dem raster data of dem
#'
#' @return raster of derived slope over dem extent
#' @keywords internal
get_slope <- function(dem) {
  slope_radians <- terra::terrain(dem, v = "slope", unit = "radians")
  tan(slope_radians)
}

#' Mask slope raster, setting the slope to zero for the pixels overlapping
#' the river area.
#'
#' @param slope raster data of slope
#' @param river vector/polygon data of river
#' @param lthresh lower numerival threshold to consider slope non-zero
#' @param target value to set for pixels overlapping river area
#'
#' @return updated slope raster
#' @keywords internal
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from the `sf` object `river`. This is used when
#'   only geometry information is needed from that point onwards and all other
#'   attributes (i.e., columns) can be safely discarded. The object returned
#'   by `sf::st_geometry()` is a simple feature geometry list column of class
#'   `sfc`.
mask_slope <- function(slope, river, lthresh = 1.e-3, target = 0) {
  slope_masked <- terra::mask(slope,
                              terra::ifel(slope <= lthresh, NA, 1),
                              updatevalue = lthresh)
  for (ngeom in seq_len(length(sf::st_geometry(river)))) {
    slope_masked <- terra::mask(slope_masked,
                                terra::vect(river[ngeom]),
                                inverse = TRUE,
                                updatevalue = target,
                                touches = TRUE)
  }
  slope_masked
}

#' Derive cost distance function from masked slope
#'
#' @param slope raster of slope data
#' @param river vector data of river
#' @param target value for cost distance calculation
#'
#' @return raster of cost distance
#' @keywords internal
#' @srrstats {SP3.1} This function uses a slope raster as a friction surface,
#'   weighting neighbour contributions continuously by slope.
get_cost_distance <- function(slope, river, target = 0) {
  slope_masked <- mask_slope(slope, river, target = target)
  cd <- terra::costDist(slope_masked, target = target)
  names(cd) <- "cost_distance"
  cd
}

#' Mask out river regions incl. a buffer in cost distance raster data
#'
#' @param cd cost distance raster
#' @param river vector/polygon
#' @param buffer size of buffer around river polygon to additionally mask
#'
#' @return cd raster with river+BUFFER pixels masked
#' @keywords internal
mask_cost_distance <- function(cd, river, buffer = 2000) {
  river_buffer <- sf::st_buffer(river, buffer) |> terra::vect()
  terra::mask(
    cd,
    river_buffer,
    updatevalue = NA,
    touches = TRUE
  )
}

#' Get characteristic value of distribution of cost distance
#'
#' @param cd cost distance raster data
#' @param method function used to derive caracteristic value (mean)
#'
#' @return characteristic value of cd raster
#' @keywords internal
#' @srrstats {G2.15} This function explicitly sets `na.rm = TRUE` when
#'   calculating the mean of a cost distance raster, which may contain `NA`
#'   values. This way, the mean is calculated only from valid raster cells,
#'   ignoring any missing values.
get_cd_char <- function(cd, method = "mean") {
  if (method == "mean") {
    mean(terra::values(cd), na.rm = TRUE)
  } else {
    stop("Not implemented!")
  }
}

#' Create vector/polygon representation of valley raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return polygon representation of valley area as object of class [`sf::sfc`]
#' @importFrom rlang .data
#' @keywords internal
#' @srrstats {G2.10} This function uses `sf::st_geometry()` to extract the
#'   geometry column from an `sf` object in a `dplyr` pipline. This is used when
#'   only geometry information is needed from that point onwards and all other
#'   attributes (i.e., columns) can be safely discarded. The object returned
#'   by `sf::st_geometry()` is a simple feature geometry list column of class
#'   `sfc`.
get_valley_polygon_raw <- function(valley_mask) {
  terra::as.polygons(valley_mask, dissolve = TRUE) |>
    sf::st_as_sf() |>
    dplyr::filter(.data$cost_distance == 1) |>
    sf::st_geometry()
}

#' Remove possible holes from valley geometry
#'
#' @param valley_polygon Geometry of valley as object of class [`sf::sfc`]
#'
#' @return (multi)polygon geometry of valley
#' @keywords internal
get_valley_polygon_no_hole <- function(valley_polygon) {
  valley_polygon |>
    sf::st_cast("POLYGON") |>
    lapply(function(x) x[1]) |>
    sf::st_multipolygon() |>
    sf::st_sfc(crs = sf::st_crs(valley_polygon))
}

#' Create vector/polygon representation of valley without holes from raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return (multi)polygon representation of valley area as a simple feature
#'   geometry without holes
#' @keywords internal
get_valley_polygon <- function(valley_mask) {
  get_valley_polygon_raw(valley_mask) |>
    get_valley_polygon_no_hole()
}
