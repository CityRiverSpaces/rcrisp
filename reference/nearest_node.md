# Find the node in a network that is closest to a target geometry.

Find the node in a network that is closest to a target geometry.

## Usage

``` r
nearest_node(network, target)
```

## Arguments

- network:

  A network object of class
  [`sfnetworks::sfnetwork`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html)

- target:

  An object of class
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

## Value

A node in the network as an object of class
[`sf::sfc_POINT`](https://r-spatial.github.io/sf/reference/sfc.html)
