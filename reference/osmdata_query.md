# Query the Overpass API for a key:value pair within a bounding box

Query the Overpass API for a key:value pair within a bounding box

## Usage

``` r
osmdata_query(key, value, bb)
```

## Arguments

- key:

  A character string with the key to filter the data

- value:

  A character string with the value to filter the data. If value = ""
  means that you get all features available in OSM for the specified
  bounding box

- bb:

  A bounding box, in lat/lon coordinates

## Value

An
[`osmdata::osmdata`](https://docs.ropensci.org/osmdata/reference/osmdata.html)
object with the retrieved OpenStreetMap data
