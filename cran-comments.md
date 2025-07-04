## R CMD check results

0 errors | 0 warnings | 1 note

Days since last update: 2

## Resubmission

This is a resubmission in response to a problem pointed out on CRAN.
In this version:

* `check_cache()` is moved from `.onLoad()` to `.onAttach()` to comply with
  CRAN policy on namespace loading.
* The NOTE on namespace load is resolved.
* In addition, the CRAN badge is added to the README, the DOI from Zenodo is
  added to CITATION.cff, DESCRIPTION and README (badge), and vignette file
  names are updated to ensure they are listed in order on CRAN.
