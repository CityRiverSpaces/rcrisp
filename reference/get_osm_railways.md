# Get OpenStreetMap railways

Get OpenStreetMap railways

## Usage

``` r
get_osm_railways(
  aoi,
  crs = NULL,
  railway_values = "rail",
  force_download = FALSE
)
```

## Arguments

- aoi:

  Area of interest as sf object or bbox

- crs:

  A numeric vector of length one with the EPSG code of the CRS

- railway_values:

  A case-insensitive character vector with the railway values to
  retrieve.

- force_download:

  Download data even if cached data is available

## Value

An object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
crs <- get_utm_zone(bb)
get_osm_railways(aoi = bb, crs = crs)
}
```
