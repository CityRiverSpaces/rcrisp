# Access digital elevation model (DEM) for a given region

Access digital elevation model (DEM) for a given region

## Usage

``` r
get_dem(
  bb,
  dem_source = "STAC",
  stac_endpoint = NULL,
  stac_collection = NULL,
  crs = NULL,
  force_download = FALSE
)
```

## Arguments

- bb:

  A bounding box, provided either as a matrix (rows for "x", "y",
  columns for "min", "max") or as a vector ("xmin", "ymin", "xmax",
  "ymax"), in lat/lon coordinates (WGS84 coordinate reference system) of
  class `bbox`

- dem_source:

  Source of the DEM:

  - If "STAC" (default), DEM tiles are searched on a SpatioTemporal
    Asset Catalog (STAC) end point, then accessed and mosaicked to the
    area of interest

- stac_endpoint:

  URL of the STAC API endpoint (only used if `dem_source` is `"STAC"`).
  For more info, see
  [`get_stac_asset_urls()`](https://cityriverspaces.github.io/rcrisp/reference/get_stac_asset_urls.md)

- stac_collection:

  Identifier of the STAC collection to be queried (only used if
  `dem_source` is `"STAC"`). For more info, see
  [`get_stac_asset_urls()`](https://cityriverspaces.github.io/rcrisp/reference/get_stac_asset_urls.md)

- crs:

  Coordinate reference system (CRS) which to transform the DEM to

- force_download:

  Download data even if cached data is available

## Value

DEM as a terra `SpatRaster` object

## Examples

``` r
if (FALSE) { # interactive()
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
}
```
