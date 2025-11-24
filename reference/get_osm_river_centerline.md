# Get the river centreline from OpenStreetMap

Get the river centreline from OpenStreetMap

## Usage

``` r
get_osm_river_centerline(bb, river_name, crs = NULL, force_download = FALSE)
```

## Arguments

- bb:

  Bounding box of class `bbox`

- river_name:

  The name of the river as character vector of length 1, case-sensitive.

- crs:

  Coordinate reference system as EPSG code

- force_download:

  Download data even if cached data is available

## Value

The river centreline as object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTILINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html).

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
crs <- get_utm_zone(bb)
get_osm_river_centerline(bb = bb, river_name = "Dâmbovița", crs = crs,
              force_download = FALSE)
}
```
