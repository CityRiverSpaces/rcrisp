# Retrieve OpenStreetMap data for a given location

Retrieve OpenStreetMap data for a given location, including the city
boundary, the river centreline and surface, the streets, the railways,
and the buildings

## Usage

``` r
get_osmdata(
  city_name,
  river_name,
  network_buffer = NULL,
  buildings_buffer = NULL,
  city_boundary = TRUE,
  crs = NULL,
  force_download = FALSE
)
```

## Arguments

- city_name:

  The name of the city as character vector of length 1, case-sensitive.
  Required, no default.

- river_name:

  The name of the river as character vector of length 1, case-sensitive.
  Required, no default.

- network_buffer:

  Buffer distance in meters around the river to get the streets and
  railways, default is 0 means no network data will be downloaded

- buildings_buffer:

  Buffer distance in meters around the river to get the buildings,
  default is 0 means no buildings data will be downloaded

- city_boundary:

  A logical indicating if the city boundary should be retrieved. Default
  is TRUE.

- crs:

  An integer or character vector of length one with the EPSG code for
  the projection. If no CRS is specified, the default is the UTM zone
  for the city.

- force_download:

  Download data even if cached data is available

## Value

A list with the retrieved OpenStreetMap data sets for the given
location, as objects of class
[`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
# Set parameters
city <- "Bucharest"
river <- "Dâmbovița"
crs <- "EPSG:31600"  # National projected CRS

# Get OSM data with defaults
get_osmdata(city_name = city, river_name = river)

# Get OSM data without city boundary
get_osmdata(city_name = city, river_name = river, city_boundary = FALSE)

# Use custom network buffer to get streets and railways
get_osmdata(city_name = city, river_name = river, network_buffer = 3500)

# Use custom buffer to get buildings
get_osmdata(city_name = city, river_name = river, buildings_buffer = 150)

# Use custom CRS
get_osmdata(city_name = city, river_name = river, crs = crs)

# Avoid getting OSM data from cache
get_osmdata(city_name = city, river_name = river, force_download = TRUE)
}
```
