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
#' @srrstatsTODO {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstatsTODO {G2.3} *For univariate character input:*
#' @srrstatsTODO {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstatsTODO {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstatsTODO {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstatsTODO {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstatsTODO {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#' @srrstatsTODO {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstatsTODO {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' @srrstatsTODO {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstatsTODO {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstatsTODO {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.*
#' @srrstatsTODO {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstatsTODO {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstatsTODO {G2.14a} *error on missing data*
#' @srrstatsTODO {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstatsTODO {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstatsTODO {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstatsTODO {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
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
#' @srrstatsTODO {SP1.0} *Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/rectilinear/geometric domains, curvilinear/geographic domains, or both.*
#' @srrstatsTODO {SP1.1} *Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality.*
#' @srrstatsTODO {SP2.0} *Spatial software should only accept input data of one or more classes explicitly developed to represent such data.*
#' @srrstatsTODO {SP2.0a} *Where new classes are implemented, conversion to other common classes for spatial data in R should be documented.*
#' @srrstatsTODO {SP2.0b} *Class systems should ensure that functions error appropriately, rather than merely warning, in response to data from inappropriate spatial domains.*
#' @srrstats {SP2.1} The package only uses `sf` for representing and handling
#'   geospatial vector data.
#' @srrstatsTODO {SP2.2} *Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through:*
#' @srrstatsTODO {SP2.2a} *Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on these established packages; and*
#' @srrstatsTODO {SP2.2b} *Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established packages.*
#' @srrstatsTODO {SP2.3} *Software which accepts spatial input data in any standard format established in other R packages (such as any of the formats able to be read by [`GDAL`](https://gdal.org), and therefore by the [`sf` package](https://cran.r-project.org/package=sf)) should include example and test code which load those data in spatial formats, rather than R-specific binary formats such as `.Rds`.*
#' @srrstatsTODO {SP2.4} *Geographical Spatial Software should be compliant with version 6 or larger of* [`PROJ`](https://proj.org/), *and with* `WKT2` *representations. The primary implication, described in detail in the articles linked to above, is that:*
#' @srrstatsTODO {SP2.4a} *Software should not permit coordinate reference systems to be represented merely by so-called "PROJ4-strings", but should use at least WKT2.*
#' @srrstatsTODO {SP2.5} *Class systems for input data must contain meta data on associated coordinate reference systems.*
#' @srrstatsTODO {SP2.5a} *Software which implements new classes to input spatial data (or the spatial components of more general data) should provide an ability to convert such input objects into alternative spatial classes such as those listed above.*
#' @srrstatsTODO {SP2.6} *Spatial Software should explicitly document the types and classes of input data able to be passed to each function.*
#' @srrstatsTODO {SP2.7} *Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstatsTODO {SP2.8} *Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.*
#' @srrstatsTODO {SP2.9} *The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or return values.*
#' @srrstatsTODO {SP4.0} *Return values should either:*
#' @srrstatsTODO {SP4.0a} *Be in same class as input data, or*
#' @srrstatsTODO {SP4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstatsTODO {SP4.1} *Any aspects of input data which are included in output data (either directly, or in some transformed form) and which contain units should ensure those same units are maintained in return values.*
#' @srrstatsTODO {SP4.2} *The type and class of all return values should be explicitly documented.*
#' @srrstatsTODO {SP5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstatsTODO {SP5.1} *Implement appropriate placement of variables along x- and y-axes.*
#' @srrstatsTODO {SP5.2} *Ensure that axis labels include appropriate units.*
#' @srrstatsTODO {SP5.3} *Offer an ability to generate interactive (generally `html`-based) visualisations of results.*
#' @srrstatsTODO {SP6.0} *Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.*
#' @srrstatsTODO {SP6.1} *All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both.*
#' @srrstatsTODO {SP6.1a} *Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.*
#' @srrstatsTODO {SP6.1b} *Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data.*
#' @srrstatsTODO {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.*
#' @srrstatsTODO {SP6.3} *Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative effects of different ways of defining neighbours.*
#' @srrstatsTODO {SP6.4} *Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity.*
#' @srrstatsTODO {SP6.5} *Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtained with a non-spatial clustering algorithm.*
#' @srrstatsTODO {SP6.6} *Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from the same spatial region, rather than from spatially distinct regions.
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
#' @srrstatsNA {G3.1, G3.1a} This package does not perform covariance
#'   calculations.
#' @srrstatsNA {G5.3} This package does not return objects which explicitly
#'   contain missing (`NA`) or undefined (`NaN`, `Inf`) values.
#' @srrstatsNA {G5.4b, G5.4c} This package implements a new method.
#'
#  Not applicable spatial software standards ----
#' @srrstatsNA {SP3.0, SP3.0a, SP3.0b, SP3.1} The package does not consider
#'   spatial neighbours.
#' @srrstatsNA {SP3.2} The package does not rely on sampling from input data.
#' @srrstatsNA {SP3.3} The package does not employ regression.
#' @srrstatsNA {SP3.5, SP3.6} The package does not implement any kind of
#'   machine learning.
#' @noRd
NULL
