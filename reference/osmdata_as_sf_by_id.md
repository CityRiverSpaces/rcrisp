# Retrieve OpenStreetMap data as sf object for a specific feature type and id

Results are cached, so that new queries with the same type and id will
be loaded from disk.

## Usage

``` r
osmdata_as_sf_by_id(type, id, force_download = FALSE)
```

## Arguments

- type:

  A character string with the OSM element type ("relation", "way", or
  "node")

- id:

  A character or numeric vector of length 1 with the OSM element id

- force_download:

  Download data even if cached data is available

## Value

An
[`osmdata::osmdata`](https://docs.ropensci.org/osmdata/reference/osmdata.html)
object with the retrieved OpenStreetMap data
