# Combine river centerline and surface

Combine river centerline and surface

## Usage

``` r
combine_river_features(river_centerline, river_surface)
```

## Arguments

- river_centerline:

  River line as
  [`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
  or
  [`sf::sfc_MULTILINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)

- river_surface:

  River surface as
  [`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
  or
  [`sf::sfc_MULTIPOLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)

## Value

Combined river as
[`sf::sfc_MULTILINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
