# ggplot2 layer for a delineation object

Creates a list of
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
layers representing the delineation results of a delineation object. The
list can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object with `+`, and further layers or themes can be added on top. Base
layers (`streets`, `railways`, `river_centerline`, `river_surface`) are
not included and can be added separately with
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Usage

``` r
geom_delineation(x, extent = "corridor", legend = TRUE)
```

## Arguments

- x:

  An object of class
  [delineation](https://cityriverspaces.github.io/rcrisp/reference/delineation.md).
  This is typically the output of the
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  function.

- extent:

  The delineation layer whose bounding box sets the plot extent. One of
  `"corridor"`, `"valley"`, `"riverspace"`, or `NULL` (no restriction,
  ggplot2 fits all layers). Defaults to `"corridor"`.

- legend:

  Whether the plot should display a legend. Default is TRUE.

## Value

A list of
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
and
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
elements to be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The following layers are included (where present in `x`), in the
following order:

- `corridor` or `riverspace` (only when `corridor` is absent): drawn
  first to set the plot extent

- `valley`: light grey, transparent fill, no outlines

- `segments`: outlined polygons

- `corridor`: outlined polygon

## See also

[`plot.delineation()`](https://cityriverspaces.github.io/rcrisp/reference/plot.delineation.md)
for a base R alternative.

## Examples

``` r
if (FALSE) { # interactive()
library(ggplot2)
bd <- delineate_city_river("Bucharest", "Dâmbovița")
ggplot() + geom_delineation(bd) + theme_void()
ggplot() + geom_delineation(bd, extent = "valley") + theme_void()
}
```
