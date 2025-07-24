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
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstatsTODO {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstatsTODO {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstatsTODO {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstatsTODO {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstatsTODO {G2.14a} *error on missing data*
#' @srrstatsTODO {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstatsTODO {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstats {G3.0} The package does not compare floating point numbers for
#'   equality. All numeric equality comparisons are made between integers.
#' @srrstats {G5.2} Error and warning behaviour is fully tested.
#' @srrstats {G5.2a} Every message produced within R code by `stop()`,
#'   `warning()`, and `message()` is unique.
#' @srrstats {G5.2b} For all messages, conditions triggering them are
#'   demonstrated and the result are compared with expected values.
#' @srrstatsTODO {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstatsTODO {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsTODO {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G5.10-4.12, below).*
#' @srrstatsTODO {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' @srrstatsTODO {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstatsTODO {G5.8a} *Zero-length data*
#' @srrstatsTODO {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstatsTODO {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstatsTODO {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstatsTODO {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstatsTODO {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstatsTODO {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstatsTODO {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow:
#' @srrstatsTODO {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsTODO {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsTODO {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
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
#' @srrstatsTODO {SP2.4, SP2.4a} By using `sf` >= 0.9, this package employs the
#'   WKT system for CRS and ensures compliance with PROJ version 6+.
#' @srrstats {SP2.5} The package uses `sf` and `SpatRaster` classes for vector
#'   and raster data, respectively, both of which contain metadata on coordinate
#'   reference systems.
#' @srrstats {SP2.6, SP2.7} Spatial input classes are documented in function
#'   documentation and validated throughout the package using
#'   `checkmate::assert_*` functions to ensure input data conforms to expected
#'   types and structures.
#' @srrstatsTODO {SP2.8} *Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.*
#' @srrstatsTODO {SP2.9} *The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or return values.*
#' @srrstatsTODO {SP6.0} *Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.*
#' @srrstatsTODO {SP6.1} *All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both.*
#' @srrstatsTODO {SP6.1a} *Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.*
#' @srrstatsTODO {SP6.1b} *Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data.*
#' @srrstatsTODO {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.*
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
#' @srrstatsNA {G2.9} This package does not perform type conversions or
#'   meta-data changes leading to information loss that would require issuing
#'   diagnostic messages.
#' @srrstatsNA {G2.11, G2.12} This package does not utilize list columns or
#'   columns with non-standard class attributes in `data.frame`-like objects.
#' @srrstatsNA {G3.1, G3.1a} This package does not perform covariance
#'   calculations.
#' @srrstatsNA {G5.3} This package does not return objects which explicitly
#'   contain missing (`NA`) or undefined (`NaN`, `Inf`) values.
#' @srrstatsNA {G5.4b, G5.4c} This package implements a new method.
#'
#  Not applicable spatial software standards ----
#' @srrstatsNA {SP2.0a, SP2.0b} This package does not implement any new classes
#'   for spatial data.
#' @srrstatsNA {SP2.5a} This package does not implement new classes.
#' @srrstatsNA {SP3.0, SP3.0a, SP3.0b, SP3.1} The package does not consider
#'   spatial neighbours.
#' @srrstatsNA {SP3.2} The package does not rely on sampling from input data.
#' @srrstatsNA {SP3.3} The package does not employ regression.
#' @srrstatsNA {SP3.5, SP3.6} The package does not implement any kind of
#'   (supervised) machine learning.
#' @srrstatsNA {SP5.0, SP5.1, SP5.2, SP5.3} The package does not return any
#'   custom classes and thus does not implement a plot method nor does it offer
#'   an ability to generate interactive visualisations. The returned
#'   delineations can be used in static and interactive visualisation workflows
#'   as any other spatial data.
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
