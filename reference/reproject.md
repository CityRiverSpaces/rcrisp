# Reproject a raster or vector dataset to the specified coordinate reference system (CRS)

Reproject a raster or vector dataset to the specified coordinate
reference system (CRS)

## Usage

``` r
reproject(x, crs, ...)
```

## Arguments

- x:

  Raster (`SpatRaster`) or vector (`sf`) object

- crs:

  CRS to be projected to, provided as `numeric`, `integer` or `logical`
  vector of length one or
  [`sf::crs`](https://r-spatial.github.io/sf/reference/coerce-methods.html).
  If `numeric`, the value should be a positive number with unrestricted
  upper bound representing a valid EPSG code.

- ...:

  Optional arguments for raster or vector reproject functions

## Value

[`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html),
[`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html), or
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object reprojected to specified CRS

## Examples

``` r
# Reproject a raster to EPSG:4326
r <- terra::rast(matrix(1:12, nrow = 3, ncol = 4), crs = "EPSG:32633")
reproject(r, 4326)
#> class       : SpatRaster
#> size        : 3, 4, 1  (nrow, ncol, nlyr)
#> resolution  : 8.98078e-06, 8.98078e-06  (x, y)
#> extent      : 10.51126, 10.51129, 1.157883e-07, 2.705813e-05  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> source(s)   : memory
#> name        :     lyr.1
#> min value   :  1.003647
#> max value   : 11.989302
```
