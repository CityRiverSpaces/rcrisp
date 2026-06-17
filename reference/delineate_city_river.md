# Delineate a corridor around a river

This is a convenience function used for quick delineation. With only the
city name and river name as input, it uses default delineation
parameters, it retrieves OSM and DEM data and returns a list with all
three delineations.

## Usage

``` r
delineate_city_river(
  city_name,
  river_name,
  corridor = TRUE,
  segments = TRUE,
  riverspace = TRUE
)
```

## Arguments

- city_name:

  A character vector of length one.

- river_name:

  A character vector of length one.

- corridor:

  Whether to carry out the corridor delineation. Default is TRUE.

- segments:

  Whether to carry out the corridor segmentation. Default is TRUE.

- riverspace:

  Whether to carry out the riverspace delineation. Default is TRUE.

## Value

A list with the valley, corridor, segments, and riverspace geometries as
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
objects.

## Examples

``` r
if (FALSE) { # interactive()
delineate_city_river("Bucharest", "Dâmbovița")
}
```
