% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valley.R
\name{get_dem}
\alias{get_dem}
\title{Access digital elevation model (DEM) for a given region}
\usage{
get_dem(
  bb,
  dem_source = "STAC",
  stac_endpoint = NULL,
  stac_collection = NULL,
  crs = NULL,
  force_download = FALSE
)
}
\arguments{
\item{bb}{A bounding box, provided either as a matrix (rows for "x", "y",
columns for "min", "max") or as a vector ("xmin", "ymin", "xmax", "ymax"),
in lat/lon coordinates (WGS84 coordinate reference system) of class \code{bbox}}

\item{dem_source}{Source of the DEM:
\itemize{
\item If "STAC" (default), DEM tiles are searched on a SpatioTemporal Asset
Catalog (STAC) end point, then accessed and mosaicked to the area of
interest
}}

\item{stac_endpoint}{URL of the STAC API endpoint (only used if \code{dem_source}
is \code{"STAC"}). For more info, see \code{\link[=get_stac_asset_urls]{get_stac_asset_urls()}}}

\item{stac_collection}{Identifier of the STAC collection to be queried (only
used if \code{dem_source} is \code{"STAC"}). For more info, see
\code{\link[=get_stac_asset_urls]{get_stac_asset_urls()}}}

\item{crs}{Coordinate reference system (CRS) which to transform the DEM to}

\item{force_download}{Download data even if cached data is available}
}
\value{
DEM as a terra \code{SpatRaster} object
}
\description{
Access digital elevation model (DEM) for a given region
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Get DEM with default values
bb <- get_osm_bb("Bucharest")
crs <- 31600  # National projected CRS

# Get DEM with default values
get_dem(bb)

# Get DEM from custom STAC endpoint
get_dem(bb,
        stac_endpoint = "some endpoint",
        stac_collection = "some collection")

# Specify CRS
get_dem(bb, crs = crs)
\dontshow{\}) # examplesIf}
}
