#' @noRd
.onAttach <- function(libname, pkgname) {
  if (interactive() || getOption("rcrisp.check_cache", default = FALSE)) {
    check_cache()
  }
}
