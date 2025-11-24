# Write data to the cache directory

Write object in a serialised form (RDS) to a cache directory. For the
directory used for caching see
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
write_data_to_cache(x, filepath, wrap = FALSE, quiet = FALSE)
```

## Arguments

- x:

  Object to serialize to a file

- filepath:

  Path where to serialize x, as a character string

- wrap:

  Whether the object should be "packed" before serialization (as
  required by
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  objects)

- quiet:

  Omit message on cache file being written

## Value

`NULL` invisibly
