# Get the river centreline from OpenStreetMap

Get the river centreline from OpenStreetMap

## Usage

``` r
get_osm_river_centerline(
  bb,
  river_name,
  crs = NULL,
  buffer_distance = NULL,
  force_download = FALSE
)
```

## Arguments

- bb:

  Bounding box of class `bbox`

- river_name:

  The name of the river as character vector of length 1, case-sensitive.

- crs:

  Coordinate reference system as EPSG code

- buffer_distance:

  Optional buffer distance in metres to expand the bounding box before
  cropping the river centreline. Useful when downstream processing (e.g.
  network or DEM analysis) extends beyond the original `bb`. Defaults to
  `NULL` (no expansion).

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
