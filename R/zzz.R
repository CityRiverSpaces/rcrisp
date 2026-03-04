#' @noRd
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("rcrisp.check_cache"))) {
    options(rcrisp.run_checks = FALSE)
  }
  invisible()
}

#' @noRd
.onAttach <- function(libname, pkgname) {
  if (interactive() || getOption("rcrisp.check_cache")) {
    check_cache()
  }
}
