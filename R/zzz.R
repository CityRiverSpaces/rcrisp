#' @noRd
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("rcrisp.run_checks"))) {
    options(rcrisp.run_checks = FALSE)
  }
  invisible()
}

#' @noRd
.onAttach <- function(libname, pkgname) {
  if (interactive() || getOption("rcrisp.run_checks")) {
    check_cache()
  }
}
