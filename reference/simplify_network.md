# Simplify a spatial network by removing multiple edges and loops.

Simplify the graph, removing loops and double-edge connections following
[this
approach](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#simplify-network).
When dropping multiple edges, keep the shortest ones.

## Usage

``` r
simplify_network(network)
```

## Arguments

- network:

  A network object

## Value

A simplifed network object
