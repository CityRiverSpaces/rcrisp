# Summarise a delineation object

Computes summary statistics for a `delineation` object and returns them
as a structured list of class `summary.delineation`. Delineation layers
are reported with their area (in km²). For `segments`, the number of
features and mean area are also given. Base layers are reported with
feature counts; `river_centerline` as total length (in km) and
`river_surface` as area (in km²).

## Usage

``` r
# S3 method for class 'delineation'
summary(object, ...)
```

## Arguments

- object:

  An object of class `delineation`.

- ...:

  Not used; included for compatibility with the generic.

## Value

An object of class `summary.delineation`, which is a named list with
elements `city_name`, `river_name`, `crs`, `parameters`,
`delineation_layers`, and `base_layers`. `parameters` contains
`network_buffer`, `dem_buffer`, and `buildings_buffer` (in metres), or
`NULL` if no `aoi` was stored. Each layer entry is itself a named list
of stats, or `NULL` if the layer is absent.

## Examples

``` r
if (FALSE) { # interactive()
bd <- delineate_city_river("Bucharest", "Dâmbovița")
s <- summary(bd)
s$delineation_layers$corridor$area_km2
}
```
