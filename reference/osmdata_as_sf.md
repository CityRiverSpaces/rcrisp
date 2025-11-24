# Retrieve OpenStreetMap data as sf object

Query the Overpass API for a key:value pair within a given bounding box
(provided as lat/lon coordiates). Results are cached, so that new
queries with the same input parameters will be loaded from disk.

## Usage

``` r
osmdata_as_sf(key, value, aoi, force_download = FALSE)
```

## Arguments

- key:

  A case-insensitive character vector of length 1 with the key to filter
  the data

- value:

  A case-insensitive character vector with the value(s) to filter the
  data

- aoi:

  An area of interest, provided either as as sf object or "bbox" or as a
  vector ("xmin", "ymin", "xmax", "ymax")

- force_download:

  Download data even if cached data is available

## Value

An
[`osmdata::osmdata`](https://docs.ropensci.org/osmdata/reference/osmdata.html)
object with the retrieved OpenStreetMap data

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
osmdata_as_sf(key = "highway",
              value = "motorway",
              aoi = bb,
              force_download = FALSE)
}
```
