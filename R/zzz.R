.onAttach <- function(libname, pkgname) {
  try({
    msg <- check_cache()
    if (!is.null(msg)) packageStartupMessage(msg)
  }, silent = TRUE)
}
