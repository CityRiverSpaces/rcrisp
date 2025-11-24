# Draw the regions corresponding to the two river banks

These are constructed as single-sided buffers around the river geometry
(see
[`river_buffer()`](https://cityriverspaces.github.io/rcrisp/reference/river_buffer.md)
for the implementation and refinement steps).

## Usage

``` r
get_river_banks(river, width)
```

## Arguments

- river:

  River spatial features provided as a
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)
  or
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html)/[`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
  object.

- width:

  Width of the regions

## Value

A [`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
object with two polygon features
