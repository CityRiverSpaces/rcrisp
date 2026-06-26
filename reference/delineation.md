# The delineation class

An S3 class representing the result of a delineation as returned by
[`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md).

## Value

An object of class `delineation` containing zero or more of the spatial
layers `$valley`, `$corridor`, `$segments`, and `$riverspace` (each an
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTIPOLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
object), plus the base OSM and optionally DEM layers and an `$aoi` list
with the delineation parameters produced by
[`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md).
See
[`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
for details.

## Examples

``` r
if (FALSE) { # interactive()
bd <- delineate_city_river("Bucharest", "Dâmbovița")
class(bd)
}
```
