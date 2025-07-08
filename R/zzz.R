#' Check the cache when the package is attached
#'
#' @srrstats {G1.4a} Internal function documented in standard Roxygen format.
#' @noRd
.onAttach <- function(libname, pkgname) {
  check_cache()
}
