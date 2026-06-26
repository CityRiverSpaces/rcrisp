# Print a delineation object

Prints a compact summary of a
[delineation](https://cityriverspaces.github.io/rcrisp/reference/delineation.md)
object, showing which layers are present and their feature counts.

## Usage

``` r
# S3 method for class 'delineation'
print(x, ...)
```

## Arguments

- x:

  An object of class
  [delineation](https://cityriverspaces.github.io/rcrisp/reference/delineation.md).

- ...:

  Not used; included for compatibility with the generic.

## Value

`x`, invisibly.

## Examples

``` r
if (FALSE) { # interactive()
bd <- delineate_city_river("Bucharest", "Dâmbovița")
print(bd)
}
```
