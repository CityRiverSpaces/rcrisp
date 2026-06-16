# Remove cache files

Remove files from cache directory either before a given date or
entirely.

## Usage

``` r
clear_cache(before_date = NULL)
```

## Arguments

- before_date:

  Date before which cache files should be removed provided as object of
  class [`Date`](https://rdrr.io/r/base/Dates.html) or as a case
  dependent character vector accepted by
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)

## Examples

``` r
if (FALSE) { # interactive()
# Clear all cache
clear_cache()

# Clear cache before given date
before_date <- as.Date("1-1-1999", "%m-%d-%Y")
clear_cache(before_date = before_date)
}
```
