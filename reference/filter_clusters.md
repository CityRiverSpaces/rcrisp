# Cluster the river crossings and select the shortest crossing per cluster

Create groups of edges that are crossing the river in nearby locations,
using a density-based clustering method (DBSCAN). This is to make sure
that edges representing e.g. different lanes of the same street are
treated as part of the same crossing. For each cluster, select the
shortest edge.

## Usage

``` r
filter_clusters(crossings, river, eps = 100)
```

## Arguments

- crossings:

  Crossing edge geometries as a simple feature object

- river:

  The river geometry as a simple feature object

- eps:

  DBSCAN parameter referring to the size (radius) distance of the
  neighborhood. Should approximate the distance between edges that we
  want to consider as a single river crossing

## Value

An object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
including the shortest edge per cluster
