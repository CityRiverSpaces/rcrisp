# Get OpenStreetMap streets

Get OpenStreetMap streets

## Usage

``` r
get_osm_streets(aoi, crs = NULL, highway_values = NULL, force_download = FALSE)
```

## Arguments

- aoi:

  Area of interest as sf object or bbox. Required, no default.

- crs:

  A numeric vector of length one with the EPSG code of the CRS

- highway_values:

  A character vector with the highway values to retrieve. If left NULL,
  the function retrieves the following values: "motorway", "trunk",
  "primary", "secondary", "tertiary"

- force_download:

  Download data even if cached data is available

## Value

An object of class
[`sf::sfc_LINESTRING`](https://r-spatial.github.io/sf/reference/sfc.html)

## Examples

``` r
if (FALSE) { # interactive()
# Set parameters
bb <- get_osm_bb("Bucharest")
crs <- 31600  # National projected CRS

# Get streets with default values
get_osm_streets(aoi = bb, crs = crs)

# Specify street categories to be retrieved
get_osm_streets(aoi = bb, crs = crs, highway_values = "primary")

# Ensure that data is not retrieved from cache
get_osm_streets(aoi = bb, crs = crs, force_download = FALSE)
}
```
