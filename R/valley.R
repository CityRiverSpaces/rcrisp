#'Retrieve asset urls for the intersection of a bounding box with a 
#'remote STAC endpoint 
#' 
#' @param endpoint url of (remote) STAC endpoint
#' @param collection STAC collection to be queried
#' @param bb A bounding box (compliant with CRiSp, i.e. as a matrix with 4 elements: xmin, ymin, xmax, ymax)
#' @param limit limiting number of records to be retireved
#'
#' @return A list of urls for the assets in the collection overlapping with the specified bounding box
#' @export
get_stac_asset_urls <- function(bb, endpoint="https://earth-search.aws.element84.com/v1", collection="cop-dem-glo-30", limit=100){
    s_obj <- stac(endpoint)
    it_obj <- s_obj |>
    stac_search(collections = collection,
              bbox = as.vector(bb),
              limit = limit) |>
    get_request()
    asset_urls <- rstac::assets_url(it_obj)
}

#' retrieve STAC records (of a DEM) corresponding to a list of asset urls, crop and merge with a specified
#' bounding box to create a dem of the specified region
#' 
#' @param asset_urls a list of STAC records to be retrieved
#' @param bb A bounding box (compliant with CRiSp, i.e. as a matrix with 4 elements: xmin, ymin, xmax, ymax)
#' 
#' @return A a merged dem fromm retrieved assets cropped to the bounding box
#' @export
get_stac_dem <- function(bb, asset_urls){
    dem <- asset_urls |>
    lapply(rast) |>
    lapply(crop, as.vector(t(bb))) |>
    do.call(merge, args=_)
}

#' Load local dem from file. Optionally crop to a bounding box.
#' NOTE: User is responsible for dem and bounding box specifications being compatible
#' 
#' @param demfilepath path to dem to be loaded
#' @param bb A bounding box (compliant with CRiSp, i.e. as a matrix with 4 elements: xmin, ymin, xmax, ymax). Optional
#' 
#' @return A a merged dem from cropped to the bounding box
#' @export
get_local_dem <- function(bb, demfilepath){
    dem <- 
}

#'Load dem either from file or a STAC endpoint
#' 
#' @param resource from whcih to source dem. Can be "STAC" or "FILE".
#' if "STAC" the parameters the following parameters must/can be supplied as named parameters
#' @param endpoint STAC endpoint to use (required)
#' @param collection STAC collection to use (required)
#' @param limit limit of records to retrieve (optional)
#' if resource is "FILE" the filepath to the dem must be specified using the named parameter
#' @param demfilepath
#' 
#' @return dem 
#' @export
get_dem <- function(bb, resource="STAC",...){
    kwargs <- list(...)
    if (resource == "STAC"){
        if(length(kwargs) && !is.null(...)){
            endpoint = kwargs$endpoint
            collection = kwargs$collection
            if("limit" in names(kwargs)){
                limit=kwargs$limit
                asset_urls <- get_stac_asset_urls(bb,endpoint=endpoint,collection=collection,limit=limit)
            } else {
                asset_urls <- get_stac_asset_urls(bb,endpoint=endpoint,collection=collection)
            }
        } else {
            asset_urls <- get_stac_asset_urls(bb)
        }
        dem <- get_stac_dem(bb, asset_urls)
    } else if (resource == "FILE") {
        demfilepath = kwargs$demfilepath
        dem <- get_local_dem(bb, demfilepath)
    } else {
        #add error statement
    }
}


#'Write dem to cloud optimized GeoTiff file as specified location
#' 
#' @param dem to write to file
#' @param fpath filepath for output. If no output directory is specified (see below) fpath is parsed to determine
#' the output directory
#' @param output_directory where file should be written. If specified fpath is treated as filename only.
#' 
#' @export
dem_to_COG <- function(dem,fpath,output_directory=None,){
    if (output_directory == None){
        file_name = basename(fpath)
        directory_name = dirname(file)
    } else {
        file_name = fpath
        directory_name = output_directory
    }
    data_dir  <- here(directory_name)
    writeRaster(
    x = dem,
    filename = sprintf("%s/%s", data_dir, filename),
    filetype = "COG",
    overwrite = TRUE)
}






#'Reproject dem raster to specified coordiante reference system
#' 
#' @param dem digital elevation model
#' @param crs coordinate reference system to be projected to
#' @param specification of reprojection metod for non-matched pixels
#' 
#' @return dem as raster reprojected to crs
reproject_dem <- function(dem, crs, method="near"){
    dem_repr <- project(dem,crs,method=method)
}

#'Reproject river vector/polygon to specified coordiante reference system
#' 
#' @param river vector/polygon of river
#' @param crs coordinate reference system to be projected to
#' @param method specification of reprojection metod for non-matched pixels. see project
#' 
#' @return river polygon reprojected to specified crs
reproject_river <- function(river, crs){
    river_repr <- st_transform(river, crs=crs)
}

#'Reproject either dem raster data or river vector/polygon 
#'to specified coordiante reference system
#' 
#' @param data raster of dem or vector/polygon of river
#' @param crs coordinate reference system to be projected to
#' @param mode either 'DEM' or 'RIVER' shorthhad specification 
#' of data type for reprojection.
#' 
#' @return data reprojected to specified crs
#' @export
reproject_dem_river <- function(data, crs, mode){
    if (mode == 'DEM'){
        data_repr <- reproject_dem(data,crs)
    }else if (mode == 'RIVER') {
       data_repr <- reproject_river(data, crs)
    } else {
        #issue warning on mode selected
    }
}

#' spatially smooth dem by (window) filtering
#' 
#' @param dem raster data of dem 
#' @param method filtering function to be used, e.g. "median". As accepted by focal
#' @param window size of filter
#' 
#' @return filtered dem
#' @export
filter_dem <- function{dem, method="median", window=5}{
    dem_filtered <- focal(dem, w=window, fun=method)
    names(dem_filtered) <- "dem_filtered"
    return(dem_filtered)
}

#' Derive slope as percentage from dem
#' This makes use of the terrain function of the terra package 
#' 
#' @param dem raster data of dem 
#' 
#' @return raster of derived slope over dem extent
#' @export
get_slope_raw <- function(dem){
    slope_radians <- terrain(dem, v = "slope", unit = "radians")
    slope <- tan(slope_radians)
}

#' Mask slope raster, setting the slope to zero for the pixels overlapping 
#' the river area.
#' 
#' @param slope raster data of slope
#' @param vector/polygon data of river
#' @param lthresh lower numerival threshold to consider slope non-zero 
#' 
#' @return updated slope raster
#' @export
mask_slope <- function(slope, river, lthresh=1.e-3){
    slope_masked <- mask(
    slope,
    ifel(slope <= lthresh, NA, 1),
    updatevalue = lthresh)

    slope_masked <- mask(
    slope_masked,
    river,
    inverse = TRUE,
    updatevalue = 0,
    touches = TRUE)
}

#` Determine slope from dem rater data and mask river areas, settingf slope to 0 within.
#'
#' @param dem raster data
#' @param river vector/polygon
#' 
#' @return raster data of slope with pixels overlapping river area set to 0
#' @export
get_slope <- function(dem,river){
    slope <- get_slope_raw(dem)
    slope_masked <- mask_slope(slope,river)
}

#' Derive cost distance function from masked slope
#' 
#' @param slope_masked raster of masked slope data
#' @param target value for codt distance calculation
#' 
#' @return raster of cost distance
#' @export
get_cost_distance_ <- function(slope_masked,target=0){
    cd <- costDist(slope_masked,target=target)
    names(cd) <- "cost_distance"
    return(cd)
}

#' Mask out river regions incl. a buffer in cost distance raster data
#' 
#' @param cd cost distance raster
#' @param river vector/polygon
#' @param BUFFER size of buffer around river polygon to additionally mask
#' 
#' @return cd raster with river+BUFFER pixels masked
#' @export
mask_cost_distance <- function(cd, river, BUFFER=2000){
    BUFFER_REGION <- BUFFER  # m
    river_buffer <- st_buffer(river, BUFFER_REGION)
    cd_masked <- mask(
        cd,
        river_buffer,
        updatevalue = NA,
        touches = TRUE)
}

#' Get characteristic value of distribution of cost distance
#'
#' @param cd cost distance raster data
#' @param method function used to derive caracteristic value (mean)
#' 
#' @return characteristic value of cd raster
#' 
get_cd_char <- function(cd, method='mean'){
    if (method=='mean'){
        cd_char <- mean(values(cd), na.rm=TRUE)
    } else {
        #TODO
    }
}

#' Select valley pixels from cost distance based on threshold
#' 
#' @param cd cost distance raster
#' @param thresh threshold cost distance value below which pixels are assuemd
#' to belong to the valley
#' 
get_valley_raw <- function(cd, thresh){
    valley_mask <- (cd < thresh)
}

#' Create vector/polygon representation of valley raster mask
#' 
#' @param valley_mask raster mask of valley pixels
#' 
#' @return polygon representation of valley area as st_geometry
#' 
get_valley_polygon_raw <- function(valley_mask){
    valley_polygon <- as.polygons(valley_mask, dissolve=TRUE) |>
    st_as_sf() |>
    filter(cost_distance == 1) |>
    st_geometry()    
}

#' Remove possible holes from valley geometry
#' 
#' @param valley_polygon st_geometry of valley region
#' 
#' @return (multi)polygon geometry of valley 
get_valley_polygon_no_hole <- function(valley_polygon){
    valley_polygon_noholes <- valley_polygon |>
    st_cast("POLYGON") |>
    lapply(function(x) x[1]) |>
    st_multipolygon() |>
    st_sfc(crs = st_crs(valley_polygon))
}

#' Create vector/polygon representation of valley without holes from raster mask
#' 
#' @param valley_mask raster mask of valley pixels
#' 
#' @return (multi)polygon representation of valley area as st_geometry without holes
#' @export
get_valley_polygon <- function(valley_mask){
    val_poly_raw <- get_valley_polygon_raw(valley_mask)
    val_poly <- get_valley_polygon_no_hole(val_poly_raw)
}


#' Create vector/polygon representation of valley from dem and river polygon
#' for a crs
#' 
#' @param dem of region
#' @param river vector/polygon representation of river area
#' @param crs coordiante reference system to use
#' 
#' @return (multi)polygon representation of valley area as st_geometry without holes
#' @export
get_valley <- function(dem, rivier, crs){
    dem_repr <- reproject_dem_river(dem,crs,"DEM")
    river_repr <- reproject_dem_river(river,crs,"RIVER")
    dem_filtered <- filter_dem(dem_repr)
    slope_masked <- get_masked_slope(dem_filtered,river_repr)
    cd <- get_cost_distance(slope_masked)
    cd_masked <- mask_cost_distance(cd,river_repr)
    cd_thresh <- get_cd_char(cd_maksed)
    valley_mask <- get_valley_raw(cd_masked, cd_thresh)
    valley_polygon <- get_valley_polygon(valley_mask)
}