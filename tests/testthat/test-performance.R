#' @srrstats {G5.7} Algorithm performance tests to verify that the full
#'   pipeline completes successfully on real-world example data and performs
#'   within reasonable bounds. This verifies that:
#'   - Each step completes successfully
#'   - Performance remains within reasonable bounds (< 60s per step)
#'   - Corridor delineation isn't dramatically slower than network construction
#'   - Riverspace delineation is fast (< 5s, mostly simple geometry ops)
test_that("Full pipeline completes successfully on Bucharest/Dambovita example data within reasonable performance bounds ", {  # nolint
  skip_on_cran()
  skip_on_ci()

  bucharest_aoi <- define_aoi("Bucharest", "Dambovita")
  bucharest_osm <- get_osm_example_data()
  bucharest_dem <- get_dem_example_data()

  expect_true(all(c("streets", "railways", "buildings", "river_centerline",
                    "river_surface") %in% names(bucharest_osm)))
  expect_s4_class(bucharest_dem, "SpatRaster")
  expect_type(bucharest_aoi, "list")

  # Measure performance for each step
  step_times <- list()

  ## Step 1. delineate_valley (spatial analysis on DEM)
  step_times$delineate_valley <- system.time({
    valley_result <- delineate_valley(bucharest_dem,
                                      bucharest_osm$river_centerline)
  })

  # Step 2. as_network (street network graph construction)
  step_times$as_network <- system.time({
    network_edges <- dplyr::bind_rows(bucharest_osm$streets,
                                      bucharest_osm$railways)
    network_result <- as_network(network_edges)
  })

  # Step 3. delineate_corridor (network-based, scales with network size)
  step_times$delineate_corridor <- system.time({
    corridor_result <- delineate_corridor(
      network_result,
      bucharest_osm$river_centerline,
      max_width = bucharest_aoi$network_buffer,
      corridor_init = valley_result
    )
  })

  # Step 4. delineate_segments (polygon-based, scales with corridor complexity)
  step_times$delineate_segments <- system.time({
    corridor_buffer <- sf::st_buffer(corridor_result, 100)
    network_filtered <- rcrisp:::filter_network(network_result, corridor_buffer)
    segments_result <- delineate_segments(corridor_result,
                                          network_filtered,
                                          bucharest_osm$river_centerline)
  })

  # Step 5. delineate_riverspace (geometry-based, scales with building density)
  step_times$delineate_riverspace <- system.time({
    river_centerline_clipped <- tryCatch(
      sf::st_intersection(bucharest_osm$river_centerline,
                          sf::st_transform(bucharest_osm$aoi_buildings,
                                           bucharest_aoi$crs)),
      error = function(e) bucharest_osm$river_centerline
    )
    # Warning about uncovered river segments is expected for this test data
    river_combined <- suppressWarnings(
      rcrisp:::combine_river_features(river_centerline_clipped,
                                      bucharest_osm$river_surface)
    )
    riverspace_result <- delineate_riverspace(river_combined,
                                              bucharest_osm$buildings)
  })

  # Validate resulting geometries

  expect_s3_class(valley_result, "sfc")
  expect_s3_class(network_result, "sfnetwork")
  expect_s3_class(corridor_result, "sfc")
  expect_s3_class(segments_result, "sfc")
  expect_s3_class(riverspace_result, "sfc")

  # Segment count should be reasonable (not exploding with complexity)
  n_segments <- length(segments_result)
  expect_true(n_segments > 0 && n_segments < 500,
              info = sprintf("Unexpected number of segments: %d", n_segments))

  # ---- Performance assertions ----

  # Extract elapsed CPU time (user + system)
  elapsed_times <- lapply(step_times, function(x) x[["elapsed"]]) |>  unlist()

  # No single step should take > 60 seconds on modern hardware
  # (reasonable threshold for a single city on contemporary machines)
  expect_true(all(elapsed_times < 60),
              info = sprintf("Some steps exceeded 60s threshold: %s",
                             paste(names(elapsed_times[elapsed_times >= 60]),
                                   collapse = ", ")))

  # delineate_corridor should not be dramatically slower than as_network
  # (corridor complexity scales with network, but not linearly)
  corridor_network_ratio <- elapsed_times[["delineate_corridor"]] /
    (elapsed_times[["as_network"]] + 0.1)  # +0.1 avoids division by tiny number
  expect_true(
    corridor_network_ratio < 20,
    info = sprintf("delineate_corridor took %.1fx as long as as_network",
                   corridor_network_ratio)
  )

  # riverspace delineation should be fast (simple geometry operations)
  expect_true(elapsed_times[["delineate_riverspace"]] < 5,
              info = sprintf("delineate_riverspace took %.2fs (expected < 5s)",
                             elapsed_times[["delineate_riverspace"]]))
})
