# Identify network edges that are intersecting a geometry

Identify network edges that are intersecting a geometry

## Usage

``` r
get_intersecting_edges(network, geometry, index = FALSE)
```

## Arguments

- network:

  A spatial network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

- geometry:

  An object of class
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

- index:

  Whether to return the indices of the matchin edges or the geometries

## Value

Indices or geometries of the edges intersecting the given geometry of
class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
