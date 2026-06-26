#' srr_stats
#'
#' @srrstatsVerbose TRUE
# nolint start
#' @srrstats {G1.2} The package includes a Roadmap in the CONTRIBUTING.md file.
#' @srrstats {G1.4} The package uses [`roxygen2`](https://roxygen2.r-lib.org/)
#'   to document all functions.
#' @srrstats {G1.4a} All internal functions are documented with
#'   [`roxygen2`](https://roxygen2.r-lib.org/) and use the `@noRd` tag.
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a} Assertions are implemented throughout
#'   the package for functions which require single- or multi-valued input of a
#'   certain type using the `checkmate` package. It is made explicit in the
#'   descriptions of such parameters that they should be of the required length.
#' @srrstats {G2.2} Throughout the package, input validation is performed
#'   using vector-specific assertions from the `checkmate` package, such as
#'   `assert_character()`, `assert_numeric()`, or the more general
#'   `assert_vector()`. This ensures that parameters expected to be univariate
#'   vectors are appropriately restricted, and multivariate input (such as
#'   matrices, data frames, or lists) is prohibited.
#' @srrstats {G3.0} The package does not compare floating point numbers for
#'   equality. All numeric equality comparisons are made between integers.
#' @srrstats {G5.2} Error and warning behaviour is fully tested.
#' @srrstats {G5.2a} Every message produced within R code by `stop()`,
#'   `warning()`, and `message()` is unique.
#' @srrstats {G5.2b} For all messages, conditions triggering them are
#'   demonstrated and the result are compared with expected values.
#'
#' @srrstats {SP1.0, SP1.1} The package description explicitly states that it
#'   uses "two-dimensional spatial information [...] in a projected CRS."
#'   Although elevation information from DEM is used in valley delineation
#'   if `valley` is chosen for the initial guess of the river corridor, the
#'   overall approach cannot be considered 3D, as all calculations performed
#'   on vector data are carried out on `x` and `y` coordinates only.
#' @srrstats {SP2.0} The package only uses the modern `sf` and `SpatRaster`
#'   classes to represent geospatial *vector* and *raster* data respectively.
#' @srrstats {SP2.1} The package only uses `sf` for representing and handling
#'   geospatial vector data.
#' @srrstats {SP2.2} The output values of this package are of either class `sf`,
#'   `SpatRaster` or `sfnetwork`, and thus are fully compatible with the
#'   established `sf`, `terra` and `sfnetworks` packages, widely used in R
#'   spatial analytical workflows.
#' @srrstats {SP2.3} The package caches spatial objects retrieved from external
#'   services as RDS objects, but these are only for internal use and their
#'   direct use is not recommended.
#' @srrstats {SP2.4, SP2.4a} By using `sf` >= 0.9, this package employs the
#'   WKT system for CRS and ensures compliance with PROJ version 6+.
#' @srrstats {SP2.5} The package uses `sf` and `SpatRaster` classes for vector
#'   and raster data, respectively, both of which contain metadata on coordinate
#'   reference systems.
#' @srrstats {SP2.6, SP2.7} Spatial input classes are documented in function
#'   documentation and validated throughout the package using
#'   `checkmate::assert_*` functions to ensure input data conforms to expected
#'   types and structures.
#' @srrstats {SP6.1a} The `delineate_corridor()`, `delineate_segments()`, and
#'   `delineate_riverspace()` functions require a projected CRS. Because
#'   passing geographic (lat/lon) data to those functions would yield inaccurate
#'   results, they raise an informative error to prevent this.
# nolint end
#' @noRd
NULL

#' NA_standards
#'
#  Not applicable general software standards ---
#' @srrstatsNA {G1.5} There is no associated publication for this package yet,
#'   and no performance claims are made.
#' @srrstatsNA {G1.6} As there are no alternative implementations, no
#'   performance claims are made in this package.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} This package does not make use of factors.
#' @srrstatsNA {G2.11, G2.12} This package does not utilize list columns or
#'   columns with non-standard class attributes in `data.frame`-like objects.
#' @srrstatsNA {G2.14, G2.14a, G2.14b, G2.14c} These standards are not
#'   applicable because `mean()` is used only within the internal function
#'   `get_cd_char()` which does not provide a user interface for handling
#'   missing data.
#' @srrstatsNA {G3.1, G3.1a} This package does not perform covariance
#'   calculations.
#' @srrstatsNA {G5.3} This package does not return objects which explicitly
#'   contain missing (`NA`) or undefined (`NaN`, `Inf`) values.
#' @srrstatsNA {G5.4b, G5.4c} This package implements a new method.
#' @srrstatsNA {G5.6b} The core algorithms of this package do not involve random
#'   components.
#' @srrstatsNA {G5.7} The results of the core algorithm of this package are not
#'   expected to return predictable trends for given changes in input data
#'   properties.
#' @srrstatsNA {G5.8c} No tabular data where all fields or all columns can be NA
#'   can be used as input in any of the function of this package.
#' @srrstatsNA {G5.9, G5.9a, G5.9b} The core algorithms (cost-distance valley
#'   extraction, shortest-path corridor delineation, DBSCAN-based crossing
#'   clustering, and reycasting in riverspace delineation) are fully
#'   deterministic, so noise susceptibility tests do not apply. These standards
#'   will be reconsidered if future versions introduce randomised sampling, e.g.
#'   for viewpoint generation in `delineate_riverspace()` or corridor
#'   corridor initialisation in `delineate_corridor()`
#' @srrstatsNA {G5.12} No special requirements are needed to run extended tests.
#'
#  Not applicable spatial software standards ----
#' @srrstatsNA {SP3.0b} The package does not consider spatial neighbours in
#'   irregular spaces.
#' @srrstatsNA {SP3.2} The package does not rely on sampling from input data.
#' @srrstatsNA {SP3.3} The package does not employ regression.
#' @srrstatsNA {SP3.5, SP3.6} The package does not implement any kind of
#'   (supervised) machine learning.
#' @srrstatsNA {SP5.3} The package does not offer an ability to generate
#'   interactive visualisations. Future implementations may extend the
#'   `visualisation.R` module, e.g., with `leaflet` functionality.
#' @srrstatsNA {SP6.3, SP6.4} This package uses spatial neighbours only via the
#'   `terra::costDist()` function. Therefore, the definition and weighting of
#'   neighbours are managed by `terra`, and are not implemented or tested within
#'   this package.
#' @srrstatsNA {SP6.5} The comparison between the results from the DBSCAN
#'   clustering algorithm and a non-spatial clustering algorithm is not
#'   relevant, as spatial proximity is the main criterion for clustering.
#' @srrstatsNA {SP6.6} This package does not implement spatial ML algorithms;
#'   therefore, tests demonstrating the effects of sampling test and training
#'   data from the same spatial region are not applicable.
#' @noRd
NULL
