## R CMD check results

0 errors | 0 warnings | 1 note

## Submission

* This is a patch release that fixes the CRAN warning from the check for
unstated dependencies in `tests`. The `autotest` package, which is used in the
test suite, is now declared under `Suggests`. As `autotest` is available from
R-universe rather than CRAN, its source is declared via
`Additional_repositories`, and the corresponding test is skipped when the
package is not installed.

* The remaining note concerns the CRAN incoming feasibility check, which reports
the availability of the `Additional_repositories` entry
(https://ropensci.r-universe.dev). This is expected, as `autotest` is only
available from R-universe.
