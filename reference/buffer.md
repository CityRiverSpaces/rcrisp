# Apply a buffer region to a sf object

If the input object is in lat/lon coordinates, the buffer is
approximately applied by first transforming the object in a suitable
projected CRS, expanding it with the given buffer, and then transforming
it back to the lat/lon system.

## Usage

``` r
buffer(obj, buffer_distance, ...)
```

## Arguments

- obj:

  A sf object

- buffer_distance:

  Buffer distance in meters

- ...:

  Optional parameters passed on to
  [`sf::st_buffer()`](https://r-spatial.github.io/sf/reference/geos_unary.html)

## Value

An object of class
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
