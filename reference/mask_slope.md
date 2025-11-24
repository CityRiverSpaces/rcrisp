# Mask slope raster, setting the slope to zero for the pixels overlapping the river area.

Mask slope raster, setting the slope to zero for the pixels overlapping
the river area.

## Usage

``` r
mask_slope(slope, river, lthresh = 0.001, target = 0)
```

## Arguments

- slope:

  raster data of slope

- river:

  vector/polygon data of river

- lthresh:

  lower numerival threshold to consider slope non-zero

- target:

  value to set for pixels overlapping river area

## Value

updated slope raster
