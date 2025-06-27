## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

This is a resubmission. In this version:

* The default cache directory has been moved to the path given by
  `tools::R_user_dir()`. Checks for the cache directory size and
  outdated files are now included on package load.
* Tests have been partly rewritten, so that they use less resources and they
  complete in a reasonable amount of time.
* Tests retrieving DEM data from AWS have been either removed or mocked
  to avoid issues with the AWS API.
* Warnings in delineation tests are safely suppressed
