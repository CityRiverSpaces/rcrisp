# Clean a spatial network

Subdivide edges by [adding missing
nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#subdivide-edges),
(optionally) simplify the network (see
[`simplify_network()`](https://cityriverspaces.github.io/rcrisp/reference/simplify_network.md)),
remove
[pseudo-nodes](https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#smooth-pseudo-nodes),
and discard all but the main connected component.

## Usage

``` r
clean_network(network, simplify = TRUE)
```

## Arguments

- network:

  A network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

- simplify:

  Whether the network should be simplified with
  [`simplify_network()`](https://cityriverspaces.github.io/rcrisp/reference/simplify_network.md)

## Value

A cleaned network object of class
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
edges <- dplyr::bind_rows(bucharest_osm$streets,
                          bucharest_osm$railways)
network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
clean_network(network)
}
```
