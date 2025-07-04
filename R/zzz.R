.onAttach <- function(libname, pkgname) {
  withCallingHandlers(
    check_cache(),
    warning = function(w) {
      packageStartupMessage(conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
}
