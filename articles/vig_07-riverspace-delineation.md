# 7. Riverspace delineation

``` r

library(rcrisp)
library(sf)

bucharest_osm <- get_osm_example_data()
bucharest_dem <- get_dem_example_data()
```

River space delineation is a delineation step that uses the river and
buildings as input to generate a polygon representing the space between
the river and the first line of buildings. We will use River Dâmbovița
of Bucharest as the river and the buildings surrounding it as input.

``` r

buildings <- bucharest_osm$buildings
river <- bucharest_osm$river_surface
```

The
[`delineate_riverspace()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_riverspace.md)
function takes the building polygons and the river (including surface
polygons and centerline) as input. The river surface polygons
intersecting the river centerline are treated as part of the river,
including connected water bodies such as lakes and reservoirs.
Viewpoints are calculated from water edges where available, and from the
river centerline when river surface polygons are missing or partial. If
no river is provided, it will return an error message. If no buildings
are provided, it will return an unobstructed buffer of a given radius
with a warning message. The function returns an sf polygon.

``` r

riverspace <- delineate_riverspace(river, buildings)
```

We visualise the riverspace in a subset of segments.

``` r

riverspace_segment_2_4 <- riverspace |>
  st_intersection(bucharest_dambovita$segments[2:4])

buildings_segment_2_4 <- buildings |>
  st_sf() |>
  st_filter(bucharest_dambovita$segments[2:4], .predicate = st_intersects) |>
  st_filter(riverspace_segment_2_4, .predicate = st_intersects)

plot(riverspace_segment_2_4, col = "orange", border = NA)
plot(buildings_segment_2_4, add = TRUE)
```

![Riverspace
delineation](img/vig_07-riverspace-delineation-plot-riverspace-1.png)
