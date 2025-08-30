
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcrisp

<!-- badges: start -->

[![R-CMD-check](https://github.com/CityRiverSpaces/rcrisp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CityRiverSpaces/rcrisp/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15793526.svg)](https://doi.org/10.5281/zenodo.15793526)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcrisp)](https://CRAN.R-project.org/package=rcrisp)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![name status
badge](https://cityriverspaces.r-universe.dev/badges/:name)](https://cityriverspaces.r-universe.dev/)
[![Research Software
Directory](https://img.shields.io/badge/rsd-rcrisp-00a3e3.svg)](https://research-software-directory.org/software/crisp)
<!-- badges: end -->

rcrisp provides tools to automate the morphological delineation of
riverside urban areas following a method developed in Forgaci ([2018,
pp. 88–89](#ref-forgaci2018)). The method is based on the premise that
analyses of riverside urban phenomena are often done without a clear and
consistent spatial definition of the area of interest and that a
morphological delineation can provide a more objective and comparable
approach.

Accordingly, the method proposes a hierarchical delineation of four
spatial units: the river valley, the river corridor, the corridor
segments and the river space. These units are defined based on the
combined morphologies of the river valley and urban form. The resulting
delineations can be used in any downstream analysis of riverside urban
areas that can benefit from consistent and comparable spatial units,
including land use, accessibility, and ecosystem services assessments.

The package includes functions to delineate the river valley, the river
corridor, the corridor segments, and the river space (i.e., the area
between the riverbanks and the first line of buildings) as well as an
all-in-one function that runs all desired delineations. The package also
includes functions to download and preprocess OpenStreetMap (OSM) and
global Digital Elevation Model (DEM) data, which are required as input
data for the delineation process.

## Workflow at a glance

1.  (Optionally) get OSM and DEM base layers
2.  Run the all-in-one `delineate()` or delineation-specific
    `delineate_*()` functions to compute valley, corridor, segments,
    and/or river space
3.  Visualize/export results for downstream analysis

See the [Getting started
vignette](https://cityriverspaces.github.io/rcrisp/articles/getting-started.html)
for further details about the purpose of the package, an end-to-end
example, data requirements, and indication of use cases.

## Installation

You can install the released version of rcrisp from
[CRAN](https://cran.r-project.org) with:

``` r
install.packages("rcrisp")
```

You can install the development version of rcrisp from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("CityRiverSpaces/rcrisp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rcrisp)

# Set location parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635

# Delineate river corridor
bd <- delineate(city_name, river_name, segments = TRUE)

# Get base layers for plotting
bb <- get_osm_bb(city_name)
streets <- get_osm_streets(bb, epsg_code)$geometry
railways <- get_osm_railways(bb, epsg_code)$geometry

# Plot results
plot(bd$corridor)
plot(railways, col = "darkgrey", add = TRUE, lwd = 0.5)
plot(streets, add = TRUE)
plot(bd$segments, border = "orange", add = TRUE, lwd = 3)
plot(bd$corridor, border = "red", add = TRUE, lwd = 3)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Contributing

rcrisp is in a stable state of development, with some degree of active
subsequent development as envisioned by the primary authors.

We also look very much forward to contributions. See the [Contributing
Guide](https://github.com/CityRiverSpaces/rcrisp/blob/main/.github/CONTRIBUTING.md)
for further details.

This package is released with a [Contributor Code of
Conduct](https://github.com/CityRiverSpaces/rcrisp/blob/main/.github/CODE_OF_CONDUCT.md).
By contributing to this project you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-forgaci2018" class="csl-entry">

Forgaci, C. (2018). *Integrated urban river corridors: Spatial design
for social-ecological integration in bucharest and beyond* \[PhD
thesis\]. <https://doi.org/10.7480/abe.2018.31>

</div>

</div>
