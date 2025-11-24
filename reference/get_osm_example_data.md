# Get example OSM data

This function retrieves example OpenStreetMap (OSM) data from the Zenodo
data repository, and it can be used in examples and tests. The code used
to generate the example dataset is available at
https://github.com/CityRiverSpaces/CRiSpExampleData. Note that the
example dataset is cached locally, so that subsequent calls to the
function can load the example data from disk without having to
re-download the data.

## Usage

``` r
get_osm_example_data(force_download = FALSE)
```

## Arguments

- force_download:

  Download data even if cached data is available

## Value

A list of sf objects containing the OSM data as
[`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html) objects.

## Examples

``` r
if (FALSE) { # interactive()
get_osm_example_data(force_download = TRUE)
}
```
