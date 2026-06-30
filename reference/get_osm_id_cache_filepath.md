# Get file path where to cache results of an Overpass API query by OSM id

The function returns the file path where to serialize an osmdata_sf
object for a given OSM element type and id. The directory used is the
one returned by
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
get_osm_id_cache_filepath(type, id)
```

## Arguments

- type:

  A character string with the OSM element type (e.g. "relation", "way",
  "node")

- id:

  A character string with the OSM element id

## Value

A character string representing the file path
