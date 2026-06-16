# Delineate a corridor around a river

Delineate a corridor around a river

## Usage

``` r
delineate(
  city_name,
  river_name,
  crs = NULL,
  network_buffer = NULL,
  buildings_buffer = NULL,
  corridor_init = "valley",
  dem = NULL,
  dem_buffer = 2500,
  max_iterations = 10,
  capping_method = "shortest-path",
  angle_threshold = 100,
  corridor = TRUE,
  segments = FALSE,
  riverspace = FALSE,
  force_download = FALSE,
  ...
)
```

## Arguments

- city_name:

  A character vector of length one

- river_name:

  A character vector of length one

- crs:

  The projected Coordinate Reference System (CRS) to use. If not
  provided, the suitable Universal Transverse Mercator (UTM) CRS is
  selected

- network_buffer:

  Add a buffer (an integer in meters) around river to retrieve
  additional data (streets, railways, etc.). Default is 3000 m.

- buildings_buffer:

  Add a buffer (an integer in meters) around the river to retrieve
  additional data (buildings). Default is 100 m.

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

- dem:

  Digital elevation model (DEM) of the region (only used if
  `corridor_init` is `"valley"`)

- dem_buffer:

  Size of the buffer region (in meters) around the river to retrieve the
  DEM (only used if `corridor_init` is `"valley"` and `dem` is NULL).

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

- force_download:

  Download data even if cached data is available

- ...:

  Additional (optional) input arguments for retrieving the DEM dataset
  (see
  [`get_dem()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem.md)).
  Only relevant if `corridor_init` is `"valley"` and `dem` is NULL

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
# Set parameters
city <- "Bucharest"
river <- "Dâmbovița"

# Delineate with defaults
delineate(city, river)

# Use custom CRS
delineate(city, river, crs = "EPSG:31600")  # National projected CRS

# Use custom network buffer
delineate(city, river, network_buffer = 3500)

# Use custom buildings buffer
delineate(city, river, buildings_buffer = 150, riverspace = TRUE)

# Provide DEM as input
bucharest_dem <- get_dem_example_data()
delineate(city, river, dem = bucharest_dem)
}
```
