# Plot a delineation object

This S3 method provides a way to quickly visualise how the layers of a
delineation object fit together using base R graphics. A delineation
object typically includes the base layers `streets`, `railways`,
`river_centerline` and `river_surface`, and the delineations of the
`valley`, `corridor`, `segments`, and `riverspace`. Depending on the
delineation object, some of the delineation layers may not be present
and thus will not be plotted.

## Usage

``` r
# S3 method for class 'delineation'
plot(x, ..., legend = TRUE)
```

## Arguments

- x:

  An object of class
  [delineation](https://cityriverspaces.github.io/rcrisp/reference/delineation.md),
  which is typically the output of the
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  function.

- ...:

  Not used; included for compatibility with the generic.

- legend:

  logical. If TRUE (default), a legend is added to the plot.

## Value

`NULL`, invisibly. This function is called for its side effect of
producing a plot.

## See also

[`geom_delineation()`](https://cityriverspaces.github.io/rcrisp/reference/geom_delineation.md)
for a `ggplot2`-based alternative.

## Examples

``` r
if (FALSE) { # interactive()
bd <- delineate_city_river("Bucharest", "Dâmbovița")
plot(bd)
}
```
