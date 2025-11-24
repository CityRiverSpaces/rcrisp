# Delineate the space surrounding a river

Delineate the space surrounding a river

## Usage

``` r
delineate_riverspace(
  river,
  occluders = NULL,
  density = 1/50,
  ray_num = 40,
  ray_length = 100
)
```

## Arguments

- river:

  List with river surface and centerline

- occluders:

  Geometry of occluders

- density:

  Density of viewpoints

- ray_num:

  Number of rays as numeric vector of length one

- ray_length:

  Length of rays in meters as numeric vector of length one

## Value

Riverspace as object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
delineate_riverspace(bucharest_osm$river_surface, bucharest_osm$buildings)
}
```
