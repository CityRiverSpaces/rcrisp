# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(rcrisp)

test_check("rcrisp")

#' @srrstats {SP2.2b} The interoperability with the `sf` package is
#'   demonstrated throughout the tests in this package.
NULL
