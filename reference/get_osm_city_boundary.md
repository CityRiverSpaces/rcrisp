# Get the city boundary from OpenStreetMap

This function retrieves the city boundary from OpenStreetMap based on a
bounding box with the OSM tags "place:city" and
"boundary:administrative". The result is filtered by the city name.

## Usage

``` r
get_osm_city_boundary(
  bb,
  city_name,
  crs = NULL,
  multiple = FALSE,
  force_download = FALSE
)
```

## Arguments

- bb:

  Bounding box of class `bbox`

- city_name:

  A case-sensitive character vector of length 1 with the name of the
  city

- crs:

  Coordinate reference system as EPSG code

- multiple:

  A logical indicating if multiple city boundaries should be returned.
  By default, only the first one is returned.

- force_download:

  Download data even if cached data is available

## Value

An object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTIPOLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
with the city boundary

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
crs <- get_utm_zone(bb)
get_osm_city_boundary(bb = bb, city_name = "Bucharest", crs = crs)
}
```
