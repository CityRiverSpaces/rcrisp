# Extract the river valley from the DEM

The slope of the digital elevation model (DEM) is used as friction
(cost) surface to compute the cost distance from any grid cell of the
raster to the river. A characteristic value (default: the mean) of the
cost distance distribution in a region surrounding the river (default: a
buffer region of 2 km) is then calculated, and used to threshold the
cost-distance surface. The resulting area is then "polygonized" to
obtain the valley boundary as a simple feature geometry.

## Usage

``` r
delineate_valley(dem, river)
```

## Arguments

- dem:

  `SpatRaster` object with the digital elevation model of the region

- river:

  An object of class
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
  representing the river

## Value

River valley as a simple feature geometry of class `sfc_MULTIPOLYGON`

## Examples

``` r
if (FALSE) { # interactive()
bucharest_osm <- get_osm_example_data()
bucharest_dem <- get_dem_example_data()
delineate_valley(bucharest_dem, bucharest_osm$river_centerline)
}
```
