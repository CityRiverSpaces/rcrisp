# Retrieve OpenStreetMap data for a given location

Retrieve OpenStreetMap data for a given location, including the city
boundary, the river centreline and surface, the streets, the railways,
and the buildings

## Usage

``` r
get_osm(
  aoi,
  city_boundary = TRUE,
  network = TRUE,
  buildings = TRUE,
  force_download = FALSE
)
```

## Arguments

- aoi:

  A list of delineation parameters

- city_boundary:

  A logical indicating if the city boundary should be retrieved. Default
  is TRUE.

- network:

  A logical indicating if the spatial network should be retrieved.
  Default is TRUE.

- buildings:

  A logical indicating if buildings should be retrieved. Default is
  TRUE.

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

# Define AOI parameters
aoi <- define_aoi(city, river)

# Get OSM data with defaults
get_osm(aoi)

# Get OSM data without city boundary
get_osm(aoi, city_boundary = FALSE)

# Use custom network buffer to get streets and railways
aoi2 <- aoi
aoi2$network_buffer = 3500
get_osm(aoi2)

# Use custom buffer to get buildings
aoi3 <- aoi
aoi3$buildings_buffer = 150
get_osm(aoi3)

# Use custom CRS
aoi4 <- aoi
aoi4$crs <- "EPSG:31600"
get_osm(aoi4)

# Avoid getting OSM data from cache
get_osm(city_name = city, river_name = river, force_download = TRUE)
}
```
