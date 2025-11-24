# Get file path where to cache digital elevation model (DEM) data

The function returns the file path where to serialize a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object representing the DEM as retrieved from a set of tiles reachable
at the given URLs, cropped and merged for the given bounding box. The
directory used is the one returned by
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
get_dem_cache_filepath(tile_urls, bbox)
```

## Arguments

- tile_urls:

  URL-paths where to reach the DEM tiles

- bbox:

  A bounding box

## Value

A character string representing the file path
