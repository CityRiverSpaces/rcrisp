# Changelog

## rcrisp 1.0.0 - 2026-10-20

### Added

- A definition of “riverside urban areas” was added in the README.
- The practical usefulness of the morphological approach was explained
  in the README.
- Test coverage of the `osm` and `aoi` modules was increased.
- The “Getting started” vignette was updated with a “Why is consistent
  delineation important?” section clarifying the problem `rcrisp` is
  meant to solve in contrast with alternative approaches.
- A note was added in the “Getting started” vignette stating that the
  workflow can be replicated for any city and river where sufficient
  data are available.
- Added an algorithm performance test to verify that the full pipeline
  completes successfully on real-world example data and performs within
  reasonable bounds.
- Added a test demonstrating that
  [`reproject()`](https://cityriverspaces.github.io/rcrisp/reference/reproject.md)
  yields equivalent results regardless of whether input data are
  curvilinear (WGS84) or rectilinear (projected).
- Distance input preprocessing is now handled by the
  [`preprocess_distance()`](https://cityriverspaces.github.io/rcrisp/reference/preprocess_distance.md)
  function. This function returns a plain numeric in meters, regardless
  of whether the input is provided as a `units` object, a plain numeric
  or another vector-like class with numeric `storage.mode`.
- [`geom_delineation()`](https://cityriverspaces.github.io/rcrisp/reference/geom_delineation.md)
  was added as a `ggplot2`-based alternative to the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method,
  returning a list of
  [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  layers that can be added to a `ggplot` object with `+`.
- [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods were added
  for `delineation` objects.
  [`print()`](https://rdrr.io/r/base/print.html) gives a compact
  overview of present layers and their feature counts.
  [`summary()`](https://rdrr.io/r/base/summary.html) additionally
  reports areas (sqkm) for delineation layers and lengths (km) for
  `river_centerline`.
- A [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method was
  created for objects of class `delineation`.
- Context-specific messages are now issued in
  [`get_river_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/get_river_aoi.md),
  [`delineate_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_corridor.md),
  and
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  when lat/lon input is reprojected for buffering, and in
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  when no CRS is provided and a UTM zone is auto-selected.
- Tests were added to
  [`delineate_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_corridor.md),
  [`delineate_segments()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_segments.md),
  and
  [`delineate_riverspace()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_riverspace.md)
  to verify that geographic (lat/lon) CRS input raises an informative
  error.
- The Rbanism community badge was added in the README.
- [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  now errors early when fewer than two river crossings are found.
- rOpenSci reviewers were acknowledged in the package metadata.
- Package logo was added.
- A JOSS paper describing the package was added in `inst/joss/`, along
  with a GitHub Actions workflow to build a draft PDF.

### Fixed

- Assertion on CRS input was removed from
  [`get_osm_streets()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_streets.md).
  CRS input is now checked in
  [`as_crs()`](https://cityriverspaces.github.io/rcrisp/reference/as_crs.md)
  as in the other `get_osm_*()` functions.
- Incorrect CRS was fixed in osmdata tests.
- Missing parenthesis was added in warning text in
  [`check_cache()`](https://cityriverspaces.github.io/rcrisp/reference/check_cache.md).
- Vignette pre-compilation was updated so that srr tags dropped by
  [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html) are
  reinserted into the vignettes. Affected vignettes were also
  recompiled.
- The input `dem_source` in
  [`get_dem()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem.md)
  was made case insensitive.
- Fixed typos in test statements.
- Corrected documentation for
  [`reproject()`](https://cityriverspaces.github.io/rcrisp/reference/reproject.md):
  the `crs` parameter no longer (incorrectly) lists `logical` as an
  accepted type. `crs` accepts numeric/integer, character
  (e.g. “EPSG:4326”) or an
  [`sf::crs`](https://r-spatial.github.io/sf/reference/coerce-methods.html)
  object; passing `TRUE`/`FALSE` will fail.
- Vignette pre-compilation was updated so that srr tags dropped by
  [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html) are
  reinserted into the vignettes. Affected vignettes were also
  recompiled.
- Documentation of return values in
  [`clear_cache()`](https://cityriverspaces.github.io/rcrisp/reference/clear_cache.md)
  and
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  were made factually consistent with the code.

### Changed

- The getting started vignette was updated to use
  [`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md)
  instead of the old `delineate(city_name, river_name, ...)` call.
- The corridor delineation vignette was updated to use
  [`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md)
  instead of the old `delineate(city_name, river_name, ...)` call, and
  to reflect the current `corridor_init` parameter (replacing the old
  `method` argument). The description of the return value was updated to
  mention the `delineation` S3 class and its full set of layers.
- The corridor segmentation vignette was updated to accurately describe
  the default behaviour of
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md),
  to reference
  [`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md),
  and to remove an unused
  [`get_dem_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem_example_data.md)
  call from the setup chunk.
- All user-facing [`stop()`](https://rdrr.io/r/base/stop.html),
  [`warning()`](https://rdrr.io/r/base/warning.html), and
  [`message()`](https://rdrr.io/r/base/message.html) calls have been
  migrated to `cli` equivalents (`cli_abort()`, `cli_warn()`,
  `cli_inform()`) for consistent output.
- The `aoi_buildings` availability check in
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  was moved earlier to fail before any computation begins.
- The workflow section of the getting started vignette was updated to
  reflect the latest package design with the five-step workflow: define
  parameters, acquire data, run delineation, visualize and validate, and
  export results.
- The README was updated with a link to the getting started vignette
  when discussing the problem of consistent delineation.
- The OSM retrieval vignette was updated to use the redesigned main
  functions. UTM CRS is retrieved now implicitly as part of
  [`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md).
- Attach-time
  [`check_cache()`](https://cityriverspaces.github.io/rcrisp/reference/check_cache.md)
  was moved behind
  [`interactive()`](https://rdrr.io/r/base/interactive.html).
- `get_osmdata()` was renamed to
  [`get_osm()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm.md)
  and all other uses of `osmdata` and `osm_data` in object and function
  names have been consistently renamed to `osm` throughout the package
  to avoid confusions with the `osmdata` package.
- The workflow of
  [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  was refactored into four composable functions to reduce parameter
  complexity and improve step-by-step control:
  - [`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md)
    (new): defines the area of interest and all delineation parameters
    (CRS, buffer sizes, corridor initialisation method) from a city and
    river name.
  - `get_osmdata()` and
    [`get_dem()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem.md)
    was adapted to accept an `aoi` object returned by
    [`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md)
    instead of individual parameters.
  - [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md):
    now accepts the `aoi` object, OSM data, and DEM as separate inputs,
    with a reduced parameter signature.
- [`delineate_city_river()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_city_river.md)
  (new): convenience wrapper that runs the full workflow from city and
  river name with default parameters.
- [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  returns now an S3 object of class `delineation`.
- [`as_network()`](https://cityriverspaces.github.io/rcrisp/reference/as_network.md)
  now checks for NAs in non-geometry columns and issues an error,
  proceeds with a warning, imputes NA values, or ignores them, as
  specified by the user with a new `na_action` parameter.

### Removed

- TODOs were moved from released code to GitHub issues.

## rcrisp 0.3.2 - 2026-09-22

CRAN release: 2026-09-22

### Fixed

- The `autotest` test is now skipped when the `autotest` package is not
  installed, and `autotest` is declared in `Suggests` together with its
  `Additional_repositories` source, resolving a check warning on CRAN.

## rcrisp 0.3.1 - 2025-11-24

CRAN release: 2025-11-24

### Changed

- To make the package check more robust with respect to missing/failing
  data access, the vignettes are frozen, all tests and examples using
  example data are skipped on CRAN

## rcrisp 0.3.0 - 2025-10-13

CRAN release: 2025-10-14

### Added

- Overall workflow was documented in a new vignette and the package
  README
- Badges to Research Software Directory and R-universe were added
- Corridors are also allowed to be multipolygons when delineating
  segments
- Badge with Status at rOpenSci Software Peer Review was added

### Fixed

- Exact matches in
  [`match_osm_name()`](https://cityriverspaces.github.io/rcrisp/reference/match_osm_name.md)
  are returned before partial matches.
- Bug returning county boundary instead of city boundary was fixed.
- Bug for river with no crossings was fixed. Delineation fails with
  informative error.
- Code chunk with OSM data retrieval was disabled in getting started
  vignette.
- [`get_osm_buildings()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_buildings.md)
  does not error when given bounding box as input.
- STAC asset URL retrieval test fails gracefully on unsuccessful HTTP
  request.
- Suppress warning in wrong city name test.

### Changed

- OSM river surface retrieval was moved to a separate function.
- Small rivers for which OSM river surface is not available can still be
  delineated.
- The Code of Conduct was replaced with the rOpenSci version as part of
  finalising the rOpenSci software review process.

## rcrisp 0.2.0 - 2025-08-21

CRAN release: 2025-08-21

### Added

- Package-level documentation has been added.
- Assertions using the `checkmate` package were added to input
  parameters throughout the package.
- Examples were expanded to demonstrates the use of all parameters.
- Delineated valley has been added to the `bucharest_dambovita` example
  package data.

### Fixed

- Classes of input parameters and return values were specified.
- [`get_osm_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_example_data.md)
  and
  [`get_dem_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem_example_data.md)
  fail gracefully in examples, vignettes and tests, that is, return a
  message and NULL, if internet resource is not available.

### Changed

- Optimised OSM river retrieval by matching the river to an OSM relation
  ID via Nominatim and fetching by ID, instead of querying all waterways
  within a bounding box and filtering by name.
- Replaced [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html) throughout the
  package for improved type safety.
- Updated package metadata in `DESCRIPTION` and `codemeta.json`
- Organized function reference page into meaningful sections.

### Removed

- The `dem_to_ceg()` function was removed from the package as it is not
  used.

## Version 0.1.4 - 2025-07-04

CRAN release: 2025-07-04

### Added

- The CRAN badge has been added to the README.
- DOI is added to CITATION.cff, DESCRIPTION and README (badge)

### Fixed

- [`check_cache()`](https://cityriverspaces.github.io/rcrisp/reference/check_cache.md)
  is only run when the package is attached, not when it is loaded to
  avoid namespace issues.

### Changed

- Vignette file names are updated to ensure they are listed in order on
  CRAN.

## Version 0.1.3 - 2025-06-27

CRAN release: 2025-07-02

### Fixed

- Tests retrieving DEM data from AWS have been either removed or mocked
  to avoid issues with the AWS API.
- Warnings in delineation tests are safely suppressed.

## Version 0.1.2 - 2025-06-26

### Changed

- The default cache directory has been moved to the path given by
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). Checks
  for the cache directory size and outdated files are now included on
  package load.
- Tests in `test-delineate.R` and `test-osmdata.R` have been partly
  rewritten, so that they use less resources and they complete in a
  reasonable amount of time.

## Version 0.1.1 - 2025-06-24

### Fixed

- Examples that take too long to run are only run in interactive mode

## Version 0.1.0 - 2025-06-23

### Added

- The first version of the package is created
