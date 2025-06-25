All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Changed

- The default cache directory has been moved to the path given by `tools::R_user_dir()`.
  Checks for the cache directory size and outdated files are now included on package load.

# Version 0.1.1 - 2025-06-24

## Fixed

- Examples that take too long to run are only run in interactive mode

# Version 0.1.0 - 2025-06-23

## Added

- The first version of the package is created
