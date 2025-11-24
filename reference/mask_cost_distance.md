# Mask out river regions incl. a buffer in cost distance raster data

Mask out river regions incl. a buffer in cost distance raster data

## Usage

``` r
mask_cost_distance(cd, river, buffer = 2000)
```

## Arguments

- cd:

  cost distance raster

- river:

  vector/polygon

- buffer:

  size of buffer around river polygon to additionally mask

## Value

cd raster with river+BUFFER pixels masked
