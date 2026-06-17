# Delineate a corridor around a river

Delineate a corridor around a river

## Usage

``` r
delineate(
  aoi,
  osm,
  dem = NULL,
  corridor_init = "valley",
  max_iterations = 10,
  capping_method = "shortest-path",
  angle_threshold = 100,
  corridor = TRUE,
  segments = FALSE,
  riverspace = FALSE
)
```

## Arguments

- aoi:

  A list of delineation parameters, namely `$city_name`, `$river_name`,
  `$bb`, `$crs`, `$network_buffer`, `$dem_buffer`, and
  `$buildings_buffer`. For more info see
  [`define_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/define_aoi.md).

- osm:

  A list with OpenStreetMap data sets for the a location, as objects of
  class [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

- dem:

  Digital elevation model (DEM) of the region (only used if
  `corridor_init` is `"valley"`)

- corridor_init:

  How to estimate the initial guess of the river corridor. It can take
  the following values:

  - "valley": use the river valley boundary, as estimated from a Digital
    Elevation Model (DEM) (for more info see
    [`delineate_valley()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_valley.md))

  - numeric or integer: use a buffer region of the given size (in
    meters) around the river centerline

  - An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) or
    [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)
    object: use the given input geometry

- max_iterations:

  Maximum number of iterations employed to refine the corridor edges
  (see
  [`corridor_edge()`](https://cityriverspaces.github.io/rcrisp/reference/corridor_edge.md)).

- capping_method:

  The method employed to connect the corridor edge end points (i.e., to
  "cap" the corridor), as character vector of length one. See
  [`cap_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/cap_corridor.md)
  for the available methods.

- angle_threshold:

  Only network edges forming angles above this threshold (in degrees)
  are considered when forming segment edges. See
  [`delineate_segments()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_segments.md)
  and
  [`rcoins::stroke()`](https://cityriverspaces.github.io/rcoins/reference/stroke.html).
  Only used if `segments` is TRUE.

- corridor:

  Whether to carry out the corridor delineation

- segments:

  Whether to carry out the corridor segmentation

- riverspace:

  Whether to carry out the riverspace delineation

## Value

A list containing zero or more of the following elements: "valley",
"corridor", "segments", and "riverspace", each as an
[`sf::sfc_POLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
or
[`sf::sfc_MULTIPOLYGON`](https://r-spatial.github.io/sf/reference/sfc.html)
object (depending on the geometry of the input data). The list contains
only the geometries corresponding to the delineation steps that were
carried out (e.g., if `segments` is FALSE, the list will not contain a
"segments" element).

## Examples

``` r
if (FALSE) { # interactive()
# Define delineation parameters within area of interest
aoi <- define_aoi("Bucharest", "Dâmbovița")

# Get data
osm <- get_osm(aoi)
dem <- get_dem(aoi, osm)

# Delineate with defaults
delineate(aoi, osm, dem)

# Carry out all delineations
delineate(aoi, osm, dem, segments = TRUE, riverspace = TRUE)
}
```
