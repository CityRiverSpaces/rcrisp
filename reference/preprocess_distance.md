# Pre-process one-dimensional distance input

Ensure that distance input is plain numeric in meters, regardless of
whether it is provided as a `units` object (e.g., a distance in
kilometers), a plain numeric or another vector-like object with numeric
`storage.mode`.

## Usage

``` r
preprocess_distance(x, arg_name = deparse(substitute(x)))
```

## Arguments

- x:

  A distance value which may be `numeric`, or a
  [`units::units`](https://r-quantities.github.io/units/reference/units.html)
  object, or any other object for which the underlying `storage.mode` is
  numeric.

- arg_name:

  Name of the argument, used in messages/errors.

## Value

A vector of class `numeric` in meters.
