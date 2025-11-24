# Find shortest path between a pair of nodes in the network.

Find shortest path between a pair of nodes in the network.

## Usage

``` r
shortest_path(network, from, to, weights = "weight")
```

## Arguments

- network:

  A spatial network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

- from:

  Start node

- to:

  End node

- weights:

  Name of the column in the network edge table from where to take the
  weigths

## Value

An
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
object
