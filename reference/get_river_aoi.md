# Get an area of interest (AoI) around a river, cropping to the bounding box of a city

Get an area of interest (AoI) around a river, cropping to the bounding
box of a city

## Usage

``` r
get_river_aoi(river, city_bbox, buffer_distance)
```

## Arguments

- river:

  A [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html) object
  with the river centreline and (optionally) the river surface geometry

- city_bbox:

  Bounding box of class `bbox` around the city

- buffer_distance:

  A positive number representing the buffer size around the river in
  meters. The upper limit is unrestricted.

## Value

An
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
object in lat/lon coordinates

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
river_centerline <- get_osm_river_centerline(bb, "Dâmbovița")
river_surface <- get_osm_river_surface(bb, "Dâmbovița")
river <- list(centerline = river_centerline, surface = river_surface)
get_river_aoi(river = river, city_bbox = bb, buffer_distance = 100)
}
```
