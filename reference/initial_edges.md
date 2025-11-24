# Identify the initial edges of the river corridor

These are defined by splitting the initial corridor boundary into the
sub-regions that the river forms in the area of interest

## Usage

``` r
initial_edges(corridor_initial, regions)
```

## Arguments

- corridor_initial:

  A simple feature geometry representing the area of the initial
  corridor

- regions:

  A simple feature geometry representing the sub-regions formed by
  cutting the area of interest along the river

## Value

An
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTILINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
object representing the initial corridor edges
