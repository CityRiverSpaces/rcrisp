# Add weights to the network.

This is to prepare the network for the search of shortest paths between
node pairs. The computed weights can account for edge lenghts, distance
from a target geometry, and whether or not an edge falls within a
specified region, which we aim to exclude from search of the shortest
paths.

## Usage

``` r
add_weights(
  network,
  target = NULL,
  exclude_area = NULL,
  penalty = 1000,
  weight_name = "weight"
)
```

## Arguments

- network:

  A network object

- target:

  Target geometry to calculate distances from, as a simple feature
  geometry

- exclude_area:

  Area that we aim to exclude from the shortest-path search, as a simple
  feature geometry

- penalty:

  Penalty (in the network CRS' units) that is added to the edges that
  falls within the excluded area

- weight_name:

  Name of the column in the edge table where to add the weights

## Value

A network object of class
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
with weights added as a column in the edge table

## Details

For the i-th edge of the network, its weight \\w_i\\ is defined in the
following way: \$\$ w_i = \|e_i\| + d\_{geom}(e_i) + p\_{buf}(e_i) \$\$
where the first term is the edge length, the second one is the distance
from a target geometry (`target`, optional) and the last one is a
penalty that is added if the centroid of the edge falls within a
specified region (`exclude_area`, optional).

Shortest paths calculated on the resulting network will thus tend to
prefer edges close to `target` and to avoid edges within `exclude_area`.
