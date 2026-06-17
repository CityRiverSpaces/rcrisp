# Define delineation parameters within an area of interest

Define delineation parameters within an area of interest

## Usage

``` r
define_aoi(
  city_name,
  river_name,
  crs = NULL,
  network_buffer = 3000,
  dem_buffer = 2500,
  buildings_buffer = 100
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

- dem_buffer:

  Size of the buffer region (in meters) around the spatial network to
  retrieve the DEM.

- buildings_buffer:

  Add a buffer (an integer in meters) around the river to retrieve
  additional data (buildings). Default is 100 m.

## Value

A list with delineation parameter values, namely `city_name` and
`river_name` as character vectors of length one, `bb` as object of class
[`sf::bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html),
`crs` as object of class
[`sf::crs`](https://r-spatial.github.io/sf/reference/coerce-methods.html),
and `network_buffer`, `dem_buffer` and `buildings_buffer` as numerical
vector of length one.

## Examples

``` r
if (FALSE) { # interactive()
# Get default parameters in AOI
aoi <- define_aoi("Bucharest", "Dâmbovița")

# Get parameters in AOI with custom CRS
aoi <- define_aoi("Bucharest", "Dâmbovița",
                  crs = "EPSG:3844")  # National projected CRS


# Get parameters in AOI with non-default buffers
aoi <- define_aoi("Bucharest", "Dâmbovița",
                  network_buffer = 2000,
                  dem_buffer = 2000,
                  buildings_buffer = 150)
}
```
