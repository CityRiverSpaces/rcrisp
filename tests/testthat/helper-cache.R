#' Fixture to setup temporary cache directory for tests
temp_cache_dir <- function(env = parent.frame()) {
  cache_dir <- file.path(tempdir(), "CRiSp-test-cache")

  # create temporary cache directory
  dir.create(cache_dir, recursive = TRUE)
  cache_dir <- normalizePath(cache_dir)
  withr::defer(unlink(cache_dir, recursive = TRUE), env)

  # set environment variable
  current_value <- Sys.getenv("CRISP_CACHE_DIRECTORY", unset = NA)
  Sys.setenv(CRISP_CACHE_DIRECTORY = cache_dir)
  withr::defer(
    {
      if (is.na(current_value)) {
        Sys.unsetenv("CRISP_CACHE_DIRECTORY")
      } else {
        Sys.setenv(CRISP_CACHE_DIRECTORY = current_value)
      }
    },
    env
  )

  cache_dir
}