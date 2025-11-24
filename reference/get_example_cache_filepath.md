# Get file path where to cache example data files

The function returns the file path where to store example data files
that are retrieved from a data repository (see
[`get_osm_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_example_data.md)
and
[`get_dem_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem_example_data.md)).
The directory used is the one returned by
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
get_example_cache_filepath(filename)
```

## Arguments

- filename:

  The name of the file to be retrieved from the data repository

## Value

A character string representing the file path
