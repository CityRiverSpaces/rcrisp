# Subset a network keeping the components that intersect a target geometry.

If subsetting results in multiple disconnected components, we keep the
main one.

## Usage

``` r
filter_network(network, target, elements = "nodes")
```

## Arguments

- network:

  A spatial network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

- target:

  The target geometry

- elements:

  The elements of the network to filter. It can be "nodes" or "edges"

## Value

A spatial network object of class
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
