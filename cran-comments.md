## R CMD check results

0 errors | 0 warnings | 0 note

## Resubmission

* This is a resubmission to solve problems identified by package checks on CRAN.
Specifically, code chunk evaluation was disabled in the network preparation
vignette to prevent failed http requests on CRAN.

* We also noticed in the rendering of all vignettes a warning originating from
`rmarkdown`. A recent release of `rmarkdown` (v2.30) addresses this issue and
thus we expect that the warning will be resolved when that version is used in
the checks on CRAN.

* One of the CRAN checks also shows a dependency error, stating that `osmdata`
is not available. However, `osmdata` is on CRAN and we believe this to be a
temporary issue on CRAN's side. (Note that the `osmdata` check results form the
failing flavor "r-devel-linux-x86_64-fedora-gcc" seem to be unavailable:
https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-fedora-gcc/osmdata-00check.html
