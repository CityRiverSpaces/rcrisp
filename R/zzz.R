.onAttach <- function(libname, pkgname) {
  msg <- check_cache()
  if (!is.null(msg)) packageStartupMessage(msg)
}
