# Read data from the cache directory

For the directory used for caching see
[`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md).

## Usage

``` r
read_data_from_cache(filepath, unwrap = FALSE, quiet = FALSE)
```

## Arguments

- filepath:

  Path of the file to deserialize as a character string

- unwrap:

  Whether the deserialized object should be "unpacked" (as required by
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  objects)

- quiet:

  Omit message on cache file being loaded

## Value

Object deserialized
