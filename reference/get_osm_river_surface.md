# Get the river surface from OpenStreetMap

Get the river surface from OpenStreetMap

## Usage

``` r
get_osm_river_surface(bb, river_centerline, crs = NULL, force_download = FALSE)
```

## Arguments

- bb:

  Bounding box of class `bbox`

- river_centerline:

  The river centerline as an object of class
  [`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
  or
  [`sf::sfc_MULTILINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)

- crs:

  Coordinate reference system as EPSG code

- force_download:

  Download data even if cached data is available

## Value

The river surface as object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTIPOLYGON`](https://r-spatial.github.io/sf/reference/sfc.html).

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
crs <- get_utm_zone(bb)
river <- get_osm_river_centerline(bb, "Dâmbovița")
get_osm_river_surface(bb = bb, river_centerline = river, crs = crs,
              force_download = FALSE)
}
```
