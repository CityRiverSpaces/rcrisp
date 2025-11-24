# Return the cache directory used by the package

By default, the user-specific directory as returned by
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) is used. A
different directory can be used by setting the environment variable
`CRISP_CACHE_DIRECTORY`. This can also be done by adding the following
line to the `.Renviron` file:
`CRISP_CACHE_DIRECTORY=/path/to/crisp/cache/dir`.

## Usage

``` r
cache_directory()
```

## Value

The cache directory used by rcrisp.

## Examples

``` r
if (FALSE) { # interactive()
cache_directory()
}
```
