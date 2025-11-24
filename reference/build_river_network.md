# Build a spatial network from river centerlines

If a bounding box is provided, only the river segments that intersect it
are considered. If the river intersects the bounding box multiple times,
only the longest intersecting segment will be considered.

## Usage

``` r
build_river_network(river, bbox = NULL)
```

## Arguments

- river:

  A (MULTI)LINESTRING simple feature geometry representing the river
  centerline

- bbox:

  Bounding box of the area of interest

## Value

An
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
object
