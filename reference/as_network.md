# Create a network from a collection of line strings

Create a network from a collection of line strings

## Usage

``` r
as_network(edges, flatten = TRUE, clean = TRUE)
```

## Arguments

- edges:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
  object with the network edges

- flatten:

  Whether all intersections between edges should be converted to nodes

- clean:

  Whether general cleaning tasks should be run on the generated network
  (see
  [`clean_network()`](https://cityriverspaces.github.io/rcrisp/reference/clean_network.md)
  for the description of tasks)

## Value

An
[`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
object

## Examples

``` r
edges <- sf::st_sfc(
  sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
  sf::st_linestring(matrix(c(0, 1, 1, 0), ncol = 2, byrow = TRUE)),
  crs = sf::st_crs("EPSG:32635")
)

# Run with default values
as_network(edges)
#> # A sfnetwork with 5 nodes and 4 edges
#> #
#> # CRS:  EPSG:32635 
#> #
#> # An unrooted tree with spatially explicit edges
#> #
#> # Node data: 5 × 1 (active)
#>             x
#>   <POINT [m]>
#> 1       (0 0)
#> 2   (0.5 0.5)
#> 3       (1 1)
#> 4       (0 1)
#> 5       (1 0)
#> #
#> # Edge data: 4 × 3
#>    from    to                x
#>   <int> <int> <LINESTRING [m]>
#> 1     1     2   (0 0, 0.5 0.5)
#> 2     2     3   (0.5 0.5, 1 1)
#> 3     2     4   (0 1, 0.5 0.5)
#> # ℹ 1 more row

# Only build the spatial network
as_network(edges, flatten = FALSE, clean = FALSE)
#> # A sfnetwork with 4 nodes and 2 edges
#> #
#> # CRS:  EPSG:32635 
#> #
#> # An unrooted forest with 2 trees with spatially explicit edges
#> #
#> # Node data: 4 × 1 (active)
#>             x
#>   <POINT [m]>
#> 1       (0 0)
#> 2       (1 1)
#> 3       (0 1)
#> 4       (1 0)
#> #
#> # Edge data: 2 × 3
#>    from    to                x
#>   <int> <int> <LINESTRING [m]>
#> 1     1     2       (0 0, 1 1)
#> 2     3     4       (0 1, 1 0)
```
