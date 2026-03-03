# Running this script pre-compiles vignettes, creating the .Rmd files from the
# corresponding .Rmd.orig ones. The procedure generates the figures, which
# should end up in the `./img` folder (see `knitr::opts_chunk` settings in each
# .Rmd.orig file).
#
# NOTE: pre-existing vignettes will not be compiled, remove the .Rmd files to
# re-generate them!
vigs_orig <- list.files("vignettes",
                        pattern = "\\.Rmd.orig",
                        full.names = TRUE)

for (vig_orig in vigs_orig) {
  vig <- gsub(".orig", "", vig_orig)
  # Only knit the vignettes that are missing
  if (!file.exists(vig)) {
    knitr::knit(vig_orig, vig)
  } else {
    message(paste("Vignette", vig, "already exists! Skipping it ..."))
  }
}
