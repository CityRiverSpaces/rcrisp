# Get OpenStreetMap buildings

Get buildings from OpenStreetMap within a given buffer around a river.

## Usage

``` r
get_osm_buildings(aoi, crs = NULL, force_download = FALSE)
```

## Arguments

- aoi:

  Area of interest as sf object or bbox

- crs:

  Coordinate reference system as EPSG code

- force_download:

  Download data even if cached data is available

## Value

An object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
crs <- get_utm_zone(bb)
get_osm_buildings(aoi = bb, crs = crs)
}
```
