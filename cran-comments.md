## R CMD check results

0 errors | 0 warnings | 1 note

Days since last update: 2

## Resubmission

This is a resubmission in response to a problem pointed out on CRAN.
In this version:

* `check_cache()` is moved from `.onLoad()` to `.onAttach()` to comply with
  CRAN policy on namespace loading.
* The NOTE on namespace load is resolved.
* Additionally, the CRAN badge is added to the README, and the DOI is added to
  CITATION.cff, DESCRIPTION and README (badge).
