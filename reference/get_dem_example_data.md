# Get example DEM data

This function retrieves example Digital Elevation Model (DEM) data from
the Zenodo data repository, and it can be used in examples and tests.
The code used to generate the example dataset is available at
https://github.com/CityRiverSpaces/CRiSpExampleData. Note that the
example dataset is cached locally, so that subsequent calls to the
function can load the example data from disk without having to
re-download the data.

## Usage

``` r
get_dem_example_data(force_download = FALSE)
```

## Arguments

- force_download:

  Download data even if cached data is available

## Value

An object of class
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
containing the DEM data.

## Examples

``` r
if (FALSE) { # interactive()
get_dem_example_data(force_download = TRUE)
}
```
