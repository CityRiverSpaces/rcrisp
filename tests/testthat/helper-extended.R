skip_if_not_extended <- function() {
  env_var <- as.logical(Sys.getenv("RCRISP_EXTENDED_TESTS", unset = "false"))
  skip_if_not(!is.na(env_var) && env_var, "Skipping extended test")
}