# Clip lines to the extent of the corridor, and select valid segment edges

Lines that intersect the river and that cross the corridor from side to
side are considered valid segment edges. We group valid segment edges
that cross the river in nearby locations, and select the shortest line
per cluster. From these candidate segment edges, we select the ultimate
set of non-intersecting lines by dropping the longest segments with most
intersections.

## Usage

``` r
clip_and_filter(lines, corridor, river)
```

## Arguments

- lines:

  Candidate segment edges as a simple feature geometry

- corridor:

  The river corridor as a simple feature geometry

- river:

  The river centerline as a simple feature geometry

## Value

Candidate segment edges as object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
