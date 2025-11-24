# Draw a corridor edge on the spatial network.

The corridor edge is drawn on the network as a shortest-path link
between a start- and an end-point. The weights in the shortest-path
problem are set to account for a) network edge lengths, b) distance from
an initial target edge geometry, and c) an excluded area where corridor
edges are aimed not to go through. The procedure is iterative, with the
excluded area only being accounted for in the first iteration. The
identified corridor edge is used as target edge in the following
iteration, with the goal of prioritising the "straightening" of the edge
(some overlap with the excluded area is allowed).

## Usage

``` r
corridor_edge(
  network,
  end_points,
  target_edge,
  exclude_area = NULL,
  max_iterations = 10
)
```

## Arguments

- network:

  The spatial network used for the delineation

- end_points:

  Target start- and end-point

- target_edge:

  Target edge geometry to follow in the delineation

- exclude_area:

  Region that we aim to exclude from the delineation

- max_iterations:

  Maximum number of iterations employed to refine the corridor edges

## Value

An
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
object representing the edge
