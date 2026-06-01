# Running this script pre-compiles vignettes, creating the .Rmd files from the
# corresponding .Rmd.orig ones. The procedure generates the figures, which
# should end up in the `./img` folder (see `knitr::opts_chunk` settings in each
# .Rmd.orig file).
#
# NOTE: pre-existing vignettes will not be compiled, remove the .Rmd files to
# re-generate them!
vigs_orig <- list.files(".", pattern = "\\.Rmd.orig")

# Extract all srr chunks from an .Rmd file as a character vector of lines.
# Chunks with eval=FALSE, echo=FALSE are silently dropped by knitr::knit(), so
# we retrieve them from the .Rmd.orig files and reinsert them after knitting.
extract_srr_chunks <- function(lines) {
  result <- character(0)
  in_srr_chunk <- FALSE
  chunk_lines <- character(0)

  for (line in lines) {
    if (grepl("^```\\{r srr", line)) {
      in_srr_chunk <- TRUE
      chunk_lines <- line
    } else if (in_srr_chunk) {
      chunk_lines <- c(chunk_lines, line)
      if (grepl("^```\\s*$", line)) {
        result <- c(result, chunk_lines, "")
        in_srr_chunk <- FALSE
        chunk_lines <- character(0)
      }
    }
  }
  result
}

for (vig_orig in vigs_orig) {
  vig <- gsub(".orig", "", vig_orig)
  # Only knit the vignettes that are missing
  if (!file.exists(vig)) {
    knitr::knit(vig_orig, vig)

    # Reinsert srr chunks that are lost during knitting
    orig_lines <- readLines(vig_orig)
    srr_chunks <- extract_srr_chunks(orig_lines)

    if (length(srr_chunks) > 0) {
      vig_lines <- readLines(vig)

      # Insert right after the "generated from .orig" comment line
      comment_pos <- grep("generated from the \\*\\.Rmd\\.orig", vig_lines)
      if (length(comment_pos) > 0) {
        insert_pos <- comment_pos[1]
      } else {
        # Fall back: after the closing --- of the YAML front matter
        yaml_markers <- which(vig_lines == "---")
        insert_pos <- if (length(yaml_markers) >= 2) yaml_markers[2] else 1L
      }

      vig_lines <- c(
        vig_lines[seq_len(insert_pos)],
        "",
        srr_chunks,
        vig_lines[(insert_pos + 1L):length(vig_lines)]
      )
      writeLines(vig_lines, vig)
    }
  } else {
    message(paste("Vignette", vig, "already exists! Skipping it ..."))
  }
}
