# 4. Valley delineation

In a typical rcrisp workflow, valley delineation is used in the context
of corridor delineation. When the parameter `method = "valley"` is set
(this is the default value) in
[`delineate_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_corridor.md),
first the river valley is extracted from a Digital Elevation Model (DEM)
with
[`delineate_valley()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_valley.md)
and then the resulting valley edge is used to “guide” the delineation of
the corridor on the street network, as shown in
`vignette("corridor-delineation")`.

In this article, we describe how
[`delineate_valley()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_valley.md)
works and how it can be used independently.
[`delineate_valley()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_valley.md)
uses a Cost Distance algorithm, variants of which are mostly used for
the delineation of wet area mapping and valley bottom delineation in
non-urban contexts (Ågren et al., 2014; Murphy et al., 2009; White et
al., 2012).

As the resulting valley boundary is only used as an intermediate step in
[`delineate_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_corridor.md),
valley delineation does not require high-resolution DEM data, as
required for non-urban applications (e.g., Lidberg et al., 2020; Nardi
et al., 2019). By default, the valley is delineated on the openly
available 30m-resolution [Copernicus DEM
GLO-30](https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM).

We demonstrate valley delineation using data from rcrisp example data,
namely the DEM of Bucharest as the input raster and the river centerline
and surface as the source for which the Cost Distance is calculated.

``` r

# Attach required packages
library(rcrisp)
library(sf)

bucharest_osm <- get_osm_example_data()
bucharest_dem <- get_dem_example_data()

# Load data for valley delineation
dem <- bucharest_dem
river_centerline <- st_geometry(bucharest_osm$river_centerline)
river_surface <- st_geometry(bucharest_osm$river_surface)
river <- c(river_centerline, river_surface)
```

![DEM of the area enclosing River Dâmbovița in
Bucharest](img/vig_04-valley-delineation-plot-dem-1.png)

DEM of the area enclosing River Dâmbovița in Bucharest

``` r

valley <- delineate_valley(dem, river)
```

![Valley polygon derived from the
DEM](img/vig_04-valley-delineation-plot-valley-1.png)

Valley polygon derived from the DEM

## References

Ågren, A. M., Lidberg, W., Strömgren, M., Ogilvie, J., & Arp, P. A.
(2014). Evaluating digital terrain indices for soil wetness mapping – a
Swedish case study. *Hydrology and Earth System Sciences*, *18*(9),
3623–3634. <https://doi.org/10.5194/hess-18-3623-2014>

Lidberg, W., Nilsson, M., & Ågren, A. (2020). Using machine learning to
generate high-resolution wet area maps for planning forest management: A
study in a boreal forest landscape. *Ambio*, *49*(2), 475–486.
<https://doi.org/10.1007/s13280-019-01196-9>

Murphy, P. N. C., Ogilvie, J., & Arp, P. (2009). Topographic modelling
of soil moisture conditions: A comparison and verification of two
models. *European Journal of Soil Science*, *60*(1), 94–109.
<https://doi.org/10.1111/j.1365-2389.2008.01094.x>

Nardi, F., Annis, A., Di Baldassarre, G., Vivoni, E. R., & Grimaldi, S.
(2019). GFPLAIN250m, a global high-resolution dataset of Earth’s
floodplains. *Scientific Data*, *6*(1), 180309.
<https://doi.org/10.1038/sdata.2018.309>

White, B., Ogilvie, Campbell, Hiltz, Gauthier, Chisholm, Wen, Murphy, &
and Arp, P. A. A. (2012). Using the Cartographic Depth-to-Water Index to
Locate Small Streams and Associated Wet Areas across Landscapes.
*Canadian Water Resources Journal / Revue Canadienne Des Ressources
Hydriques*, *37*(4), 333–347. <https://doi.org/10.4296/cwrj2011-909>
