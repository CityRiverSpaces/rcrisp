# Get the file path where to cache results of an Overpass API query

The function returns the file path where to serialize an osdata_sf
object for a given key:value pair and a bounding box. The directory used
is the one returned by
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
get_osm_cache_filepath(key, value, bbox)
```

## Arguments

- key:

  A character string with the key to filter the data

- value:

  A character string with the value to filter the data

- bbox:

  A bounding box

## Value

A character string representing the file path
