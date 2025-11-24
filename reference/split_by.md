# Split a geometry along a (multi)linestring.

Split a geometry along a (multi)linestring.

## Usage

``` r
split_by(geometry, line, boundary = FALSE)
```

## Arguments

- geometry:

  Geometry to split

- line:

  Dividing (multi)linestring

- boundary:

  Whether to return the split boundary instead of the regions

## Value

An object of class
[`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
