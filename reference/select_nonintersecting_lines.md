# Select non-intersecting line segments

Recursively drop intersecting lines, starting from the line that form
most intersections with other geometries. When multilple lines form the
same number of intersections with other geometries, the longest line is
discarded first. Note that lines are allowed to intersect on the
corridor boundary.

## Usage

``` r
select_nonintersecting_lines(lines, corridor)
```

## Arguments

- lines:

  Candidate edge segment as a simple feature geometry

- corridor:

  The river corridor as a simple feature geometry

## Value

A set of lines of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)
that do not intersect within the corridor geometry, as a simple feature
geometry
