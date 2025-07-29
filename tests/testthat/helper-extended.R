skip_if_not_extended <- function() {
  env_var <- as.logical(Sys.getenv("RCRISP_EXTENDED_TESTS", unset = "false"))
  testthat::skip_if(is.na(env_var) || !env_var, "Skipping extended test")
}
