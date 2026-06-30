# Look up a river's OSM relation via Nominatim

Look up a river's OSM relation via Nominatim

## Usage

``` r
nominatim_waterway_lookup(river_name)
```

## Arguments

- river_name:

  A character string with the river name

## Value

A data frame with Nominatim results filtered to waterway river
relations, or an empty data frame if none found.
