# Find the corridor end points.

Determine the extremes (end points) of the river corridor using the
network built from the river center line features (see
[`build_river_network()`](https://cityriverspaces.github.io/rcrisp/reference/build_river_network.md)
and the spatial network used for the delineation. The end points are
selected as the two furthest river crossings of the spatial network that
connect the sub-networks for each river sides.

## Usage

``` r
corridor_end_points(river_network, spatial_network, regions)
```

## Arguments

- river_network:

  A
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
  object representing the river centerline

- spatial_network:

  A
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
  object representing the spatial network used for the delineation

- regions:

  A simple feature geometry representing the two river sides

## Value

An [`sf::sfc_POINT`](https://r-spatial.github.io/sf/reference/sfc.html)
geometry including a pair of points
