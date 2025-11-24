# Flatten a network by adding points at apparent intersections

All crossing edges are identified, and the points of intersections are
injected within the edge geometries. Note that the injected points are
not converted to network nodes (this can be achieved via sfnetworks'
[`sfnetworks::to_spatial_subdivision()`](https://luukvdmeer.github.io/sfnetworks/reference/spatial_morphers.html),
which is part of the tasks that are included in
[`clean_network()`](https://cityriverspaces.github.io/rcrisp/reference/clean_network.md).

## Usage

``` r
flatten_network(network)
```

## Arguments

- network:

  A network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

## Value

An
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
object with additional points at intersections

## Details

The functionality is similar to sfnetworks'
[`sfnetworks::st_network_blend()`](https://luukvdmeer.github.io/sfnetworks/reference/st_network_blend.html),
but in that case an external point is only injected to the closest edge.

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
edges <- dplyr::bind_rows(bucharest_osm$streets,
                          bucharest_osm$railways)
network <- sfnetworks::as_sfnetwork(edges, directed = FALSE)
flatten_network(network)
}
```
