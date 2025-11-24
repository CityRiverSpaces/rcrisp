# Cap the corridor by connecting the edge end points

Cap the corridor by connecting the edge end points

## Usage

``` r
cap_corridor(edges, method = "shortest-path", network = NULL)
```

## Arguments

- edges:

  A simple feature geometry representing the corridor edges

- method:

  The method employed for the capping:

  - `shortest-path` (default): find the network-based shortest-path
    connections between the edge end points.

  - `direct`: connect the start points and the end points of the edges
    via straight segments

- network:

  A spatial network object, only required if `method = 'shortest-path'`

## Value

An
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
object representing the corridor
