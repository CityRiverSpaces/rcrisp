# Retrieve DEM data from a list of STAC assets

Load DEM data from a list of tiles, crop and merge using a given
bounding box to create a raster DEM for the specified region. Results
are cached, so that new queries with the same input parameters will be
loaded from disk.

## Usage

``` r
load_dem(bb, tile_urls, force_download = FALSE)
```

## Arguments

- bb:

  A bounding box, provided either as a matrix (rows for "x", "y",
  columns for "min", "max") or as a vector ("xmin", "ymin", "xmax",
  "ymax") of class `bbox`.

- tile_urls:

  A list of tiles where to read the DEM data from

- force_download:

  Download data even if cached data is available

## Value

A DEM of class
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
retrieved and retiled to the given bounding box

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
tile_urls <- get_stac_asset_urls(bb)
load_dem(bb = bb, tile_urls = tile_urls, force_download = TRUE)
}
```
