# Spatially smooth dem by (window) filtering

Spatially smooth dem by (window) filtering

## Usage

``` r
smooth_dem(dem, method = "median", window = 5)
```

## Arguments

- dem:

  raster data of dem

- method:

  smoothing function to be used, e.g. "median", as accepted by
  [`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html)

- window:

  size of smoothing kernel

## Value

filtered dem
