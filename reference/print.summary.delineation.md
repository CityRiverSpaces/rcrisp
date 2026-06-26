# Print a summary.delineation object

Print a summary.delineation object

## Usage

``` r
# S3 method for class 'summary.delineation'
print(x, ...)
```

## Arguments

- x:

  An object of class `summary.delineation`, as returned by
  [`summary.delineation()`](https://cityriverspaces.github.io/rcrisp/reference/summary.delineation.md).

- ...:

  Not used; included for compatibility with the generic.

## Value

`x`, invisibly.

## Examples

``` r
if (FALSE) { # interactive()
bd <- delineate_city_river("Bucharest", "Dâmbovița")
s <- summary(bd)
print(s)
}
```
