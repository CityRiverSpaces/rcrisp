All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

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
