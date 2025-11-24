# Delineate a river corridor on a spatial network

The corridor edges on the two river banks are drawn on the provided
spatial network starting from an initial guess of the corridor (based
e.g. on the river valley).

## Usage

``` r
delineate_corridor(
  network,
  river,
  corridor_init = 1000,
  max_width = 3000,
  max_iterations = 10,
  capping_method = "shortest-path"
)
```

## Arguments

- network:

  The spatial network of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
  to be used for the delineation. Required, no default.

- river:

  A (MULTI)LINESTRING simple feature geometry of class
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
  representing the river centerline. Required, no default.

- corridor_init:

  How to estimate the initial guess of the river corridor. It can take
  the following values:

  - numeric or integer: use a buffer region of the given size (in
    meters, positive, unrestricted) around the river centerline

  - An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
    [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
    object: use the given input geometry

- max_width:

  A positive number representing the (approximate) maximum width of the
  corridor in meters. The upper limit is unrestricted. The spatial
  network is trimmed by a buffer region of this size around the river.

- max_iterations:

  A positive integer greater than 0, with upper limit unrestricted,
  representing the maximum number of iterations employed to refine the
  corridor edges (see
  [`corridor_edge()`](https://cityriverspaces.github.io/rcrisp/reference/corridor_edge.md)).

- capping_method:

  Case-insensitive character vector of length 1 with the method employed
  to connect the corridor edge end points (i.e. to "cap" the corridor).
  See
  [`cap_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/cap_corridor.md)
  for the available methods.

## Value

A simple feature geometry of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
representing the river corridor

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
streets <- bucharest_osm$streets
railways <- bucharest_osm$railways
river <- bucharest_osm$river_centerline

# Delineate with default values
network <- rbind(streets, railways) |> as_network()
delineate_corridor(network = network, river = river)

# Delineate with user-specified parameters
bucharest_dem <- get_dem_example_data()
corridor_init <- delineate_valley(dem = bucharest_dem, river = river)
delineate_corridor(network = network, river = river,
                   corridor_init = corridor_init,
                   max_width = 4000, max_iterations = 5,
                   capping_method = "direct")
}
```
