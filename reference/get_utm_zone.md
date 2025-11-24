# Get the UTM zone of a spatial object

Get the UTM zone of a spatial object

## Usage

``` r
get_utm_zone(x)
```

## Arguments

- x:

  Object in any class accepted by
  [`as_bbox()`](https://cityriverspaces.github.io/rcrisp/reference/as_bbox.md)

## Value

The EPSG code of the UTM zone

## Examples

``` r
# Get EPSG code for UTM zone of Bucharest
bb <- c(xmin = 25.97, ymin = 44.33, xmax = 26.23, ymax = 44.54)
get_utm_zone(bb)
#> [1] 32635
```
