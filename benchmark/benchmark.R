# ---- Set alternative Overpass API URL
library(osmdata)
url <- "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
set_overpass_url(url)

# ---- Attach packages
library(rcrisp)
library(sf)
library(dplyr)

# ---- Configuration ----------------------------------------------------------

INPUT_CSV    <- "benchmark/data/city_rivers_selection.csv"
OUTPUT_DIR   <- "benchmark/output"
TIMINGS_CSV  <- file.path(OUTPUT_DIR, "timings.csv")
MAX_RETRIES  <- 3
RETRY_WAIT_S <- 30  # seconds between retries

# ---- Helpers ----------------------------------------------------------------

# Run expr, return list(result, elapsed_s, status, error)
timed_step <- function(expr) {
  start  <- proc.time()[["elapsed"]]
  result <- tryCatch(
    {
      val <- force(expr)
      list(result = val, status = "success", error = NA_character_)
    },
    error = function(e) {
      list(result = NULL, status = "error", error  = conditionMessage(e))
    }
  )
  elapsed <- proc.time()[["elapsed"]] - start
  result$elapsed_s <- elapsed
  result
}

# Retry wrapper for download steps
with_retry <- function(
  expr_fn, max_retries = MAX_RETRIES, wait_s = RETRY_WAIT_S, label = "step"
) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    out <- timed_step(expr_fn())
    if (out$status == "success" || attempt >= max_retries) return(out)
    cat(sprintf(
      "  [retry %d/%d] %s failed: %s. Waiting %ds ...\n",
      attempt, max_retries, label, out$error, wait_s
    ))
    Sys.sleep(wait_s)
  }
}

# Make a safe filename component from an arbitrary string
safe_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

# Append a row to a running timings data.frame
record_timing <- function(timings, city, river, step, out) {
  rbind(timings, data.frame(
    city_name    = city,
    river_name   = river,
    step         = step,
    elapsed_s    = round(out$elapsed_s, 3),
    status       = out$status,
    error        = if (is.na(out$error)) "" else out$error,
    stringsAsFactors = FALSE
  ))
}

# ---- Load input data --------------------------------------------------------

city_rivers <- read.csv(
  INPUT_CSV,
  header = FALSE,
  col.names = c("city_name", "river_name"),
  colClasses = "character",
  encoding = "UTF-8",
  strip.white = TRUE
)

cat(sprintf(
  "Loaded %d city-river pairs from %s\n\n", nrow(city_rivers), INPUT_CSV
))

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

timings <- data.frame(
  city_name = character(),
  river_name = character(),
  step = character(),
  elapsed_s = numeric(),
  status = character(),
  error = character(),
  stringsAsFactors = FALSE
)

# ---- Main loop --------------------------------------------------------------

total_start <- proc.time()[["elapsed"]]

for (i in seq_len(nrow(city_rivers))) {

  city  <- city_rivers$city_name[i]
  river <- city_rivers$river_name[i]
  label <- sprintf("[%d/%d] %s – %s", i, nrow(city_rivers), city, river)

  cat(rep("=", nchar(label) + 4), "\n", sep = "")
  cat("  ", label, "\n")
  cat(rep("=", nchar(label) + 4), "\n", sep = "")

  # -- Overall timer for this city-river pair
  pair_start <- proc.time()[["elapsed"]]

  # -- 1. define_aoi (not benchmarked as a separate step; pure in-memory)
  aoi_out <- timed_step(define_aoi(city, river))
  if (aoi_out$status == "error") {
    cat("  ERROR (define_aoi):", aoi_out$error, "\n\n")
    timings <- record_timing(timings, city, river, "define_aoi", aoi_out)
    next
  }
  aoi <- aoi_out$result

  # -- 2. get_osm (with retry)
  cat("  get_osm ...\n")
  osm_out <- with_retry(
    function() get_osm(aoi, force_download = TRUE),
    label = "get_osm"
  )
  timings <- record_timing(timings, city, river, "get_osm", osm_out)
  if (osm_out$status == "error") {
    cat("  ERROR (get_osm):", osm_out$error, "\n\n")
    next
  }
  osm <- osm_out$result

  # -- 3. get_dem (with retry)
  cat("  get_dem ...\n")
  dem_out <- with_retry(
    function() get_dem(aoi, osm, force_download = TRUE),
    label = "get_dem"
  )
  timings <- record_timing(timings, city, river, "get_dem", dem_out)
  if (dem_out$status == "error") {
    cat("  ERROR (get_dem):", dem_out$error, "\n\n")
    next
  }
  dem <- dem_out$result

  # -- 4. delineate_valley
  cat("  delineate_valley ...\n")
  valley_out <- timed_step(delineate_valley(dem, osm$river_centerline))
  timings <- record_timing(timings, city, river, "delineate_valley", valley_out)
  if (valley_out$status == "error") {
    cat("  ERROR (delineate_valley):", valley_out$error, "\n\n")
    next
  }
  valley <- valley_out$result

  # -- 5. as_network
  cat("  as_network ...\n")
  network_edges <- dplyr::bind_rows(osm$streets, osm$railways)
  network_out   <- timed_step(as_network(network_edges))
  timings <- record_timing(timings, city, river, "as_network", network_out)
  if (network_out$status == "error") {
    cat("  ERROR (as_network):", network_out$error, "\n\n")
    next
  }
  network <- network_out$result

  # -- 6. delineate_corridor
  cat("  delineate_corridor ...\n")
  corridor_out <- timed_step(
    delineate_corridor(
      network, osm$river_centerline,
      max_width    = aoi$network_buffer,
      corridor_init = valley
    )
  )
  timings <- record_timing(
    timings, city, river, "delineate_corridor", corridor_out
  )
  if (corridor_out$status == "error") {
    cat("  ERROR (delineate_corridor):", corridor_out$error, "\n\n")
    next
  }
  corridor <- corridor_out$result

  # -- 7. delineate_segments
  cat("  delineate_segments ...\n")
  corridor_buffer  <- sf::st_buffer(corridor, 100)
  network_filtered <- rcrisp:::filter_network(network, corridor_buffer)
  segments_out <- timed_step(
    delineate_segments(corridor, network_filtered, osm$river_centerline)
  )
  timings <- record_timing(
    timings, city, river, "delineate_segments", segments_out
  )
  if (segments_out$status == "error") {
    cat("  ERROR (delineate_segments):", segments_out$error, "\n\n")
    next
  }
  segments <- segments_out$result

  # -- 8. delineate_riverspace
  cat("  delineate_riverspace ...\n")
  river_centerline_clipped <- tryCatch(
    sf::st_intersection(
      osm$river_centerline,
      sf::st_transform(osm$aoi_buildings, aoi$crs)
    ),
    error = function(e) {
      cat(
        "WARNING: st_intersection for riverspace failed:", conditionMessage(e),
        "— using full river centerline\n"
      )
      osm$river_centerline
    }
  )
  river_combined <- rcrisp:::combine_river_features(river_centerline_clipped,
                                                    osm$river_surface)
  riverspace_out <- timed_step(
    delineate_riverspace(river_combined, osm$buildings)
  )
  timings <- record_timing(
    timings, city, river, "delineate_riverspace", riverspace_out
  )
  if (riverspace_out$status == "error") {
    cat("  ERROR (delineate_riverspace):", riverspace_out$error, "\n\n")
    next
  }
  riverspace <- riverspace_out$result

  # -- 9. Overall elapsed
  pair_elapsed <- round(proc.time()[["elapsed"]] - pair_start, 3)
  timings <- record_timing(
    timings, city, river, "total",
    list(elapsed_s = pair_elapsed, status = "success", error = NA_character_)
  )

  # -- 10. Write GeoPackage output
  gpkg_file <- file.path(
    OUTPUT_DIR,
    sprintf("%s_%s.gpkg", safe_name(city), safe_name(river))
  )

  tryCatch({
    corridor_sf  <- sf::st_as_sf(data.frame(geometry = corridor))
    segments_sf  <- sf::st_as_sf(data.frame(geometry = segments))
    riverspace_sf <- sf::st_as_sf(data.frame(geometry = riverspace))

    sf::st_write(corridor_sf,   gpkg_file, layer = "corridor",
                 delete_dsn = TRUE, quiet = TRUE)
    sf::st_write(segments_sf,   gpkg_file, layer = "segments",
                 append = TRUE, quiet = TRUE)
    sf::st_write(riverspace_sf, gpkg_file, layer = "riverspace",
                 append = TRUE, quiet = TRUE)
    cat(sprintf("  Output written to: %s\n", gpkg_file))
  }, error = function(e) {
    cat("  ERROR (write gpkg):", conditionMessage(e), "\n")
  })

  cat(sprintf("  Done in %.1fs\n\n", pair_elapsed))

  # Save timings incrementally after each city so progress is not lost
  write.csv(timings, TIMINGS_CSV, row.names = FALSE)
}

# ---- Final summary ----------------------------------------------------------

total_elapsed <- round(proc.time()[["elapsed"]] - total_start, 1)
cat(sprintf("\nAll done. Total elapsed: %.1fs\n", total_elapsed))
cat(sprintf("Timings saved to: %s\n", TIMINGS_CSV))

write.csv(timings, TIMINGS_CSV, row.names = FALSE)
