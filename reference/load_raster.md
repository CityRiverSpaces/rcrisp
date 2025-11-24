# Load raster data from one or multiple (remote) files

If a bounding box is provided, the file(s) are cropped for the given
extent. The resulting rasters are then merged using
[`terra::merge`](https://rspatial.github.io/terra/reference/merge.html).

## Usage

``` r
load_raster(urlpaths, bbox = NULL)
```

## Arguments

- urlpaths:

  Path or URL to the raster file(s)

- bbox:

  A bounding box

## Value

Raster data as a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object
