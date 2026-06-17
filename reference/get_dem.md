# Access digital elevation model (DEM) for a given region

Access digital elevation model (DEM) for a given region

## Usage

``` r
get_dem(
  aoi,
  osm,
  dem_source = "STAC",
  stac_endpoint = NULL,
  stac_collection = NULL,
  force_download = FALSE
)
```

## Arguments

- aoi:

  A list of delineation parameters, including `$dem_buffer` used to
  expand the area of interest covered by the network and `$crs` for the
  CRS which to transform the DEM to

- osm:

  A list with OpenStreetMap data sets for the a location, as objects of
  class [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

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

- force_download:

  Download data even if cached data is available

## Value

DEM as a terra `SpatRaster` object

## Examples

``` r
if (FALSE) { # interactive()
# Define delineation parameters and get OSM data within area of interest
aoi <- define_aoi("Bucharest", "Dâmbovița")
osm <- get_osm(aoi)

# Get DEM with default values
dem <- get_dem(aoi, osm)

# Get DEM from custom STAC endpoint
get_dem(aoi, osm,
        stac_endpoint = "some endpoint",
        stac_collection = "some collection")

# Specify CRS
get_dem(bb, crs = crs)
}
```
