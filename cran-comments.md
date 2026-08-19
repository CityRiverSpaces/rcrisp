## R CMD check results

0 errors | 0 warnings | 0 note

## Resubmission

* This is a resubmission to solve problems identified by package checks on CRAN.
Specifically, vignettes, tests and examples are now modified so that no remote
resources are required during CRAN checks.

* We also noticed in the rendering of all vignettes a warning originating from
`rmarkdown`. A recent release of `rmarkdown` (v2.30) addresses this issue and
thus we expect that the warning will be resolved when that version is used in
the checks on CRAN.

* One of the CRAN checks also shows a dependency error, stating that `osmdata`
is not available. However, `osmdata` is on CRAN and we believe this to be a
temporary issue on CRAN's side (see check for `r-oldrel-macos-x86_64`: https://www.r-project.org/nosvn/R.check/r-oldrel-macos-x86_64/rcrisp-00check.html).
