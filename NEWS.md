All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Added

- `geom_delineation()` was added as a `ggplot2`-based alternative to the `plot()` method, returning a list of `geom_sf()` layers that can be added to a `ggplot` object with `+`.
- `print()` and `summary()` methods were added for `delineation` objects. `print()` gives a compact overview of present layers and their feature counts. `summary()` additionally reports areas (sqkm) for delineation layers and lengths (km) for `river_centerline`.
- A `plot()` method was created for objects of class `delineation`.
- Context-specific messages are now issued in `get_river_aoi()`, `delineate_corridor()`, and `delineate()` when lat/lon input is reprojected for buffering, and in `delineate()` when no CRS is provided and a UTM zone is auto-selected.
- Tests were added to `delineate_corridor()`, `delineate_segments()`, and `delineate_riverspace()` to verify that geographic (lat/lon) CRS input raises an informative error.
- The Rbanism community badge was added in the README.

## Fixed

- Missing parenthesis was added in warning text in `check_cache()`.
- Vignette pre-compilation was updated so that srr tags dropped by `knitr::knit()` are reinserted into the vignettes. Affected vignettes were also recompiled.
- The input `dem_source` in `get_dem()` was made case insensitive.
- Fixed typos in test statements.
- Corrected documentation for `reproject()`: the `crs` parameter no longer (incorrectly) lists `logical` as an accepted type. `crs` accepts numeric/integer, character (e.g. "EPSG:4326") or an `sf::crs` object; passing `TRUE`/`FALSE` will fail.
- Vignette pre-compilation was updated so that srr tags dropped by `knitr::knit()` are reinserted into the vignettes. Affected vignettes were also recompiled.
- Documentation of return values in `clear_cache()` and `delineate()` were made factually consistent with the code.

## Changed

- The OSM retrieval vignette was updated to retrieve CRS with `get_utm_zone()`
- Attach-time `check_cache()` was moved behind `interactive()`.
- `get_osmdata()` was renamed to `get_osm()` and all other uses of `osmdata` and `osm_data` in object and function names have been consistently renamed to `osm` throughout the package to avoid confusions with the `osmdata` package.
- The workflow of `delineate()` was refactored into four composable functions to reduce parameter complexity and improve step-by-step control:
  - `define_aoi()` (new): defines the area of interest and all delineation parameters (CRS, buffer sizes, corridor initialisation method) from a
    city and river name.
  - `get_osmdata()` and `get_dem()` was adapted to accept an `aoi` object returned by `define_aoi()` instead of individual parameters.
  - `delineate()`: now accepts the `aoi` object, OSM data, and DEM as separate inputs, with a reduced parameter signature.
- `delineate_city_river()` (new): convenience wrapper that runs the full workflow from city and river name with default parameters.
- `delineate()` returns now an S3 object of class `delineation`.

## Removed

- TODOs were moved from released code to GitHub issues.

# rcrisp 0.3.1 - 2025-11-24

## Changed

- To make the package check more robust with respect to missing/failing data access, the vignettes are frozen, all tests and examples using example data are skipped on CRAN

# rcrisp 0.3.0 - 2025-10-13

## Added

- Overall workflow was documented in a new vignette and the package README
- Badges to Research Software Directory and R-universe were added
- Corridors are also allowed to be multipolygons when delineating segments
- Badge with Status at rOpenSci Software Peer Review was added

## Fixed

- Exact matches in `match_osm_name()` are returned before partial matches.
- Bug returning county boundary instead of city boundary was fixed.
- Bug for river with no crossings was fixed. Delineation fails with informative error.
- Code chunk with OSM data retrieval was disabled in getting started vignette.
- `get_osm_buildings()` does not error when given bounding box as input.
- STAC asset URL retrieval test fails gracefully on unsuccessful HTTP request.
- Suppress warning in wrong city name test.

## Changed

- OSM river surface retrieval was moved to a separate function.
- Small rivers for which OSM river surface is not available can still be delineated.

# rcrisp 0.2.0 - 2025-08-21

## Added

- Package-level documentation has been added.
- Assertions using the `checkmate` package were added to input parameters throughout the package.
- Examples were expanded to demonstrates the use of all parameters.
- Delineated valley has been added to the `bucharest_dambovita` example package data.

## Fixed

- Classes of input parameters and return values were specified.
- `get_osm_example_data()` and `get_dem_example_data()` fail gracefully in examples, vignettes and tests, that is, return a message and NULL, if internet resource is not available.

## Changed

- Replaced `sapply()` with `vapply()` throughout the package for improved type safety.
- Updated package metadata in `DESCRIPTION` and `codemeta.json`
- Organized function reference page into meaningful sections.

## Removed

- The `dem_to_ceg()` function was removed from the package as it is not used.

# Version 0.1.4 - 2025-07-04

## Added

- The CRAN badge has been added to the README.
- DOI is added to CITATION.cff, DESCRIPTION and README (badge)

## Fixed

- `check_cache()` is only run when the package is attached, not when it is loaded to avoid namespace issues.

## Changed

- Vignette file names are updated to ensure they are listed in order on CRAN.

# Version 0.1.3 - 2025-06-27

## Fixed

- Tests retrieving DEM data from AWS have been either removed or mocked to avoid issues with the AWS API.
- Warnings in delineation tests are safely suppressed.

# Version 0.1.2 - 2025-06-26

## Changed

- The default cache directory has been moved to the path given by `tools::R_user_dir()`.
  Checks for the cache directory size and outdated files are now included on package load.
- Tests in `test-delineate.R` and `test-osmdata.R` have been partly rewritten, so that they use less resources and they complete in a reasonable amount of time.

# Version 0.1.1 - 2025-06-24

## Fixed

- Examples that take too long to run are only run in interactive mode

# Version 0.1.0 - 2025-06-23

## Added

- The first version of the package is created
