# Split corridor along the river to find edges on the two banks

Split corridor along the river to find edges on the two banks

## Usage

``` r
get_corridor_edges(corridor, river)
```

## Arguments

- corridor:

  The river corridor as a simple feature geometry

- river:

  The river centerline as a simple feature geometry

## Value

Corridor edges as an object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
