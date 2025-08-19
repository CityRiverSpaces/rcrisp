## R CMD check results

0 errors | 0 warnings | 0 note

## Resubmission

* To fix the current problems on CRAN: `get_osm_example_data()` and
`get_dem_example_data()` were updated to fail gracefully in examples, vignettes
and tests, that is, return a message and NULL, if internet resource is not
available.

* In addition, this version includes extensive documentation updates, adds input
assertions throughout the package, removes an unused function, and replaces
`sapply()` with `vapply()` throughout the package.
