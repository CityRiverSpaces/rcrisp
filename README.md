
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CRiSp

<!-- badges: start -->

[![R-CMD-check](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CityRiverSpaces/CRiSp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

CRiSp (City River Spaces) provides tools to automate the morphological
delineation of riverside urban areas.

## Installation

You can install the development version of CRiSp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CityRiverSpaces/CRiSp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CRiSp)

# Set location parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635

# Get base layer for plotting
bucharest_bb <- get_osm_bb(city_name)
bucharest_streets <- get_osm_streets(bucharest_bb, epsg_code)[, "geometry"]
#> Warning in read_data_from_cache(filepath): Loading data from cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/osmdata_highway_motorway_trunk_primary_secondary_tertiary_motorway_link_trunk_link_primary_link_secondary_link_tertiary_link_25.967_44.334_26.226_44.541.rds

# Delineate river corridor
bucharest_dambovita <- delineate("Bucharest", "Dâmbovița", crs = epsg_code)
#> Warning in read_data_from_cache(filepath): Loading data from cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/osmdata_waterway_river_25.967_44.334_26.226_44.541.rds
#> Warning: attribute variables are assumed to be spatially constant throughout
#> all geometries
#> Warning in read_data_from_cache(filepath): Loading data from cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/osmdata_natural_water_25.967_44.334_26.226_44.541.rds
#> Warning in read_data_from_cache(filepath): Loading data from cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/osmdata_highway_motorway_trunk_primary_secondary_tertiary_motorway_link_trunk_link_primary_link_secondary_link_tertiary_link_25.967_44.334_26.226_44.541.rds
#> Warning in read_data_from_cache(filepath): Loading data from cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/osmdata_railway_rail_25.967_44.334_26.226_44.541.rds
#> Warning in read_data_from_cache(filepath, unwrap = TRUE): Loading data from
#> cache directory:
#> /Users/claudiuforgaci/.cache/CRiSp/dem_Copernicus_DSM_COG_10_N44_00_E026_00_DEM_Copernicus_DSM_COG_10_N44_00_E025_00_DEM_25.967_44.334_26.226_44.541.rds
#> Warning: to_spatial_subdivision assumes attributes are constant over geometries
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

# Plot results
plot(bucharest_dambovita$corridor, border = "orange", lwd = 3)
plot(bucharest_streets, add = TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contributing

We look very much forward to contributions to the package. See the
[Contributing Guide](.github/CONTRIBUTING.md) for further details.

This package is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.
