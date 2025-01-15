#' Endpoint and collection ID of the default STAC colleciton where to access
#' digital elevation model (DEM) data. This is the global Copernicus DEM 30
#' dataset hosted on AWS, as listed in the EarthSearch STAC API endpoint.
#' Note that AWS credentials need to be setup in order to access the data (not
#' the catalog).
# nolint start
#' References:
#'  - [EarthSearch STAC API](https://element84.com/earth-search/)
#'  - [Copernicus DEM](https://spacedata.copernicus.eu/collections/copernicus-digital-elevation-model)
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
#'   in lat/lon coordinates (WGS84 coordinate referece system)
#' @param dem_source Source of the DEM:
#'   - If "STAC" (default), DEM tiles are searched on a SpatioTemporal Asset
#'     Catalog (STAC) end point, then accessed and mosaicked to the area of
#'     interest
#' @param stac_endpoint URL of the STAC API endpoint (only used if `dem_source`
#'   is `"STAC"`). To be provided together with `stac_collection`, or leave
#'   blank to use defaults (see [`default_stac_dem`])
#' @param stac_collection Identifier of the STAC collection to be queried (only
#'   used if `dem_source` is `"STAC"`). To be provided together with
#'   `stac_endpoint`, or leave blank to use defaults (see [`default_stac_dem`])
#' @param crs Coordinate reference system (CRS) which to transform the DEM to
#'
#' @return DEM as a terra SpatRaster object
#' @export
get_dem <- function(bb, dem_source = "STAC", stac_endpoint = NULL,
                    stac_collection = NULL, crs = NULL) {
  bbox <- as_bbox(bb)
  if (dem_source == "STAC") {
    if (is.null(stac_endpoint) && is.null(stac_collection)) {
      stac_endpoint <- default_stac_dem$endpoint
      stac_collection <- default_stac_dem$collection
    } else if (is.null(stac_endpoint) || is.null(stac_collection)) {
      stop("Provide both or neither of `stac_endpoint` and `stac_collection`")
    }
    asset_urls <- get_stac_asset_urls(bbox, endpoint = stac_endpoint,
                                      collection = stac_collection)
    dem <- load_raster(bbox, asset_urls)
  } else {
    stop(sprintf("DEM source %s unknown", dem_source))
  }
  if (!is.null(crs)) dem <- reproject(dem, crs)
  return(dem)
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
#' @param dem Digital elevation model of the region
#' @param river A simple feature geometry representing the river
#' @param bbox Bounding box defining the extent of the area of interest
#'
#' @return River valley as a simple feature geometry
#' @export
get_valley <- function(dem, river, bbox = NULL) {
  if (!terra::same.crs(dem, sf::st_crs(river)$wkt)) {
    stop("DEM and river geometry should be in the same CRS")
  }
  if (!is.null(bbox)) {
    bbox <- as_bbox(bbox)
    dem <- terra::crop(dem, terra::ext(bbox))
  }
  cd_masked <- smooth_dem(dem) |>
    get_slope() |>
    get_cost_distance(river) |>
    mask_cost_distance(river)

  cd_thresh <- get_cd_char(cd_masked)

  valley <- get_valley_polygon(cd_masked < cd_thresh)
  return(valley)
}

#' Retrieve the URLs of all the assets intersecting a bbox from a STAC API
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax"),
#'   in lat/lon coordinates (WGS84 coordinate referece system)
#' @param endpoint URL of the STAC API endpoint
#' @param collection STAC collection to be queried
#'
#' @return A list of URLs for the assets in the collection overlapping with
#'   the specified bounding box
#' @export
get_stac_asset_urls <- function(bb, endpoint, collection) {
  bbox <- as_bbox(bb)
  it_obj <- rstac::stac(endpoint) |>
    rstac::stac_search(collections = collection, bbox = bbox) |>
    rstac::get_request()
  asset_urls <- rstac::assets_url(it_obj)
  return(asset_urls)
}

#' Retrieve STAC records (of a DEM) corresponding to a list of asset urls,
#' crop and merge with a specified bounding box to create a dem of the
#' specified region
#'
#' @param bb A bounding box, provided either as a matrix (rows for "x", "y",
#'   columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax")
#' @param raster_urlpaths a list of STAC records to be retrieved
#'
#' @return A a merged dem from retrieved assets cropped to the bounding box
#' @export
load_raster <- function(bb, raster_urlpaths) {
  bbox <- as_bbox(bb)
  raster_urlpaths |>
    lapply(terra::rast) |>
    # snap spatial extent outward to include pixels crossed by the boundary
    lapply(terra::crop, terra::ext(bbox), snap = "out") |>
    do.call(terra::merge, args = _)
}

#' Write DEM to cloud optimized GeoTiff file as specified location
#'
#' @param dem to write to file
#' @param fpath filepath for output. If no output directory is specified
#' (see below) fpath is parsed to determine
#' the output directory
#' @param output_directory where file should be written.
#' If specified fpath is treated as filename only.
#'
#' @export
dem_to_cog <- function(dem, fpath, output_directory = NULL) {
  if (is.null(output_directory)) {
    filename <- basename(fpath)
    directory_name <- dirname(fpath)
  } else {
    filename <- fpath
    directory_name <- output_directory
  }
  data_dir <- directory_name
  terra::writeRaster(
                     x = dem,
                     filename = sprintf("%s/%s", data_dir, filename),
                     filetype = "COG",
                     overwrite = TRUE)
}

#' Spatially smooth dem by (window) filtering
#'
#' @param dem raster data of dem
#' @param method smoothing function to be used, e.g. "median",
#'   as accepted by [terra::focal()]
#' @param window size of smoothing kernel
#'
#' @return filtered dem
#' @export
smooth_dem <- function(dem, method = "median", window = 5) {
  dem_smoothed <- terra::focal(dem, w = window, fun = method)
  names(dem_smoothed) <- "dem_smoothed"
  return(dem_smoothed)
}

#' Derive slope as percentage from DEM
#'
#' This makes use of the terrain function of the terra package
#'
#' @param dem raster data of dem
#'
#' @return raster of derived slope over dem extent
#' @export
get_slope <- function(dem) {
  slope_radians <- terra::terrain(dem, v = "slope", unit = "radians")
  slope <- tan(slope_radians)
  return(slope)
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
#'
#' @export
mask_slope <- function(slope, river, lthresh = 1.e-3, target = 0) {
  slope_masked <- terra::mask(slope,
                              terra::ifel(slope <= lthresh, NA, 1),
                              updatevalue = lthresh)
  slope_masked <- terra::mask(slope_masked,
                              terra::vect(river),
                              inverse = TRUE,
                              updatevalue = target,
                              touches = TRUE)
}

#' Derive cost distance function from masked slope
#'
#' @param slope raster of slope data
#' @param river vector data of river
#' @param target value for cost distance calculation
#'
#' @return raster of cost distance
#' @export
get_cost_distance <- function(slope, river, target = 0) {
  slope_masked <- CRiSp::mask_slope(slope, river, target = target)
  cd <- terra::costDist(slope_masked, target = target)
  names(cd) <- "cost_distance"
  return(cd)
}

#' Mask out river regions incl. a buffer in cost distance raster data
#'
#' @param cd cost distance raster
#' @param river vector/polygon
#' @param buffer size of buffer around river polygon to additionally mask
#'
#' @return cd raster with river+BUFFER pixels masked
#' @export
mask_cost_distance <- function(cd, river, buffer = 2000) {
  river_buffer <- sf::st_buffer(river, buffer) |> terra::vect()
  cd_masked <- terra::mask(
    cd,
    river_buffer,
    updatevalue = NA,
    touches = TRUE
  )
  return(cd_masked)
}

#' Get characteristic value of distribution of cost distance
#'
#' @param cd cost distance raster data
#' @param method function used to derive caracteristic value (mean)
#'
#' @return characteristic value of cd raster
#' @export
get_cd_char <- function(cd, method = "mean") {
  if (method == "mean") {
    cd_char <- mean(terra::values(cd), na.rm = TRUE)
    return(cd_char)
  } else {
    #TODO
  }
}

#' Create vector/polygon representation of valley raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return polygon representation of valley area as st_geometry
#' @importFrom rlang .data
#' @export
get_valley_polygon_raw <- function(valley_mask) {
  valley_polygon <- terra::as.polygons(valley_mask, dissolve = TRUE) |>
    sf::st_as_sf() |>
    dplyr::filter(.data$cost_distance == 1) |>
    sf::st_geometry()
  return(valley_polygon)
}

#' Remove possible holes from valley geometry
#'
#' @param valley_polygon st_geometry of valley region
#'
#' @return (multi)polygon geometry of valley
#' @export
get_valley_polygon_no_hole <- function(valley_polygon) {
  valley_polygon_noholes <- valley_polygon |>
    sf::st_cast("POLYGON") |>
    lapply(function(x) x[1]) |>
    sf::st_multipolygon() |>
    sf::st_sfc(crs = sf::st_crs(valley_polygon))
  return(valley_polygon_noholes)
}

#' Create vector/polygon representation of valley without holes from raster mask
#'
#' @param valley_mask raster mask of valley pixels
#'
#' @return (multi)polygon representation of valley area as a simple feature
#'   geometry without holes
#' @export
get_valley_polygon <- function(valley_mask) {
  val_poly <- CRiSp::get_valley_polygon_raw(valley_mask) |>
    CRiSp::get_valley_polygon_no_hole()
  return(val_poly)
}
