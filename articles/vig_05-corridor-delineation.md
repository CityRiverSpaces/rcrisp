# 5. Corridor delineation

``` r

library(rcrisp)
library(sf)

bucharest_osm <- get_osm_example_data()
bucharest_dem <- get_dem_example_data()
```

In this notebook we explore how to delineate an urban river corridor
using river Dâmbovița in Bucharest, Romania. We will use OpenStreetMap
(OSM) data, first from the Overpass API and then from a local file.

Corridor delineation depends on the availability of OpenStreetMap street
and railway data around the river. Sparse OSM data, especially too few
river crossings, may lead to failed delineation.

``` r

city_name <- "Bucharest"
river_name <- "Dâmbovița"
```

We start by demonstrating the use of the
[`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md)
convenience function, which runs the full workflow from city and river
name with default parameters. Internally, it carries out the following
steps:

1.  Defines the area of interest and delineation parameters with
    [`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md);

2.  Fetches city boundary, street and rail network, as well as river
    centreline and surface data from the [Overpass
    API](https://wiki.openstreetmap.org/wiki/Overpass_API) as shown in
    `vignette("getting-osm-data")`;

3.  Pre-processes the street and rail network for delineation as shown
    in `vignette("network-preparation")`;

4.  Constructs the initial corridor based on the chosen method. With the
    default `corridor_init = "valley"`, the Cost Distance Accumulation
    algorithm is used to delineate the corridor based on the digital
    elevation model (DEM) of the area retrieved from the [Earth Search
    API](https://element84.com/earth-search/). If `corridor_init` is set
    to a numeric value instead, the corridor is initialised by buffering
    the river centreline with a buffer of that distance (in metres).

5.  Delineates the corridor based on the pre-processed network and
    initial corridor. Optionally, the corridor is split into segments
    based on the network and the river space is delineated.

[`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md)
returns an object of class `delineation`, which is a named list
containing zero or more of the delineation layers `valley`, `corridor`,
`segments`, and `riverspace`, plus the base OSM layers and delineation
parameters. For step-by-step control, each of the above steps can also
be run individually using
[`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md),
[`get_osm()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm.md),
[`get_dem()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem.md),
and
[`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md).

``` r

bucharest_dambovita <- delineate_city_river(
  city_name,
  river_name,
  segments = TRUE,
  riverspace = TRUE
)
```

``` r

# Plot all layers within the extent of the delineated corridor
bbox <- st_bbox(bucharest_dambovita$corridor)
plot(bucharest_dambovita$valley, col = "grey", border = NA,
     xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))
plot(bucharest_dambovita$riverspace, col = "lightgreen", border = NA,
     add = TRUE)
plot(bucharest_osm$river_centerline, col = "blue", add = TRUE)
plot(bucharest_dambovita$segments, border = "lightblue", add = TRUE)
plot(bucharest_dambovita$corridor, border = "red", wt = 2, add = TRUE)
```

![Delineation of the corridor of River Dâmbovița in
Bucharest](img/vig_05-corridor-delineation-plot-1.png)
