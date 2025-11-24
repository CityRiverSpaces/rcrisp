# Split a river corridor into segments

Segments are defined as corridor subregions separated by river-crossing
transversal lines that form continuous strokes in the network.

## Usage

``` r
delineate_segments(corridor, network, river, angle_threshold = 100)
```

## Arguments

- corridor:

  The river corridor as a simple feature geometry of class `sfc_POLYGON`

- network:

  The spatial network of class `sfnetwork` to be used for the
  segmentation

- river:

  The river centerline as a simple feature geometry of class
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

- angle_threshold:

  Only consider angles above this threshold (in degrees) to form
  continuous strokes in the network. The value can range between 0 and
  180, with the default set to 100. See
  [`rcoins::stroke()`](https://cityriverspaces.github.io/rcoins/reference/stroke.html)
  for more details.

## Value

Segment polygons as a simple feature geometry of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
corridor <- bucharest_dambovita$corridor
network <- rbind(bucharest_osm$streets, bucharest_osm$railways) |>
  as_network()
river <- bucharest_osm$river_centerline
delineate_segments(corridor, network, river)
}
```
