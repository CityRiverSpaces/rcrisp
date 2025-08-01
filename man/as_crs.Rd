% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as_crs}
\alias{as_crs}
\title{Standardise the coordinate reference system (CRS) of an object}
\usage{
as_crs(x, allow_geographic = FALSE)
}
\arguments{
\item{x}{An object of class \code{sf}, \code{sfc}, \code{bbox}, or a numeric or character
vector representing a CRS (e.g., EPSG code). If \code{numeric}, the value
should be an unrestricted positive number representing a valid EPSG code.}

\item{allow_geographic}{Logical, whether to allow geographic CRS (lat/lon).}
}
\value{
An object of class \code{\link[sf:coerce-methods]{sf::crs}} with a valid CRS.
}
\description{
Standardise the coordinate reference system (CRS) of an object
}
\examples{
library(sf)

# Standardise a numeric EPSG code
as_crs(4326, allow_geographic = TRUE)

# Standardise a character EPSG code
as_crs("EPSG:4326", allow_geographic = TRUE)

# Standardise a bbox object
bb <- st_bbox(c(xmin = 25.9, ymin = 44.3, xmax = 26.2, ymax = 44.5),
                crs = 4326)
as_crs(bb, allow_geographic = TRUE)

# Standardise a simple feature object
bb_sfc <- st_as_sfc(bb)
bb_sf <- st_as_sf(bb_sfc)
as_crs(bb_sf, allow_geographic = TRUE)
as_crs(bb_sfc, allow_geographic = TRUE)
}
