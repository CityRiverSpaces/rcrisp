# Draw a corridor as a fixed buffer region around a river.

The river geometry may consist of multiple spatial features, these are
optionally cropped using the area of interest, then merged after
applying the buffer.

## Usage

``` r
river_buffer(river, buffer_distance, bbox = NULL, side = NULL)
```

## Arguments

- river:

  A simple feature geometry representing the river

- buffer_distance:

  Size of the buffer (in the river's CRS units)

- bbox:

  Bounding box defining the extent of the area of interest

- side:

  Whether to generate a single-sided buffer with a "flat" end. This is
  only applicable if `river` is a (multi)linestring geometry. Choose
  between `NULL` (double-sided), `"right"` and `"left"`

## Value

An object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
