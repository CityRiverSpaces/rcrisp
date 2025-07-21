x <- delineate("Longyearbyen", "Longyearelva", corridor_init = 1000)
filepath <- testthat::test_path("testdata",
                                "expected_corridor_longyearbyen.gpkg")
sf::st_write(x$corridor, filepath, append = FALSE, quiet = TRUE)
