# Query the Overpass API for a specific feature type and id

Query the Overpass API for a specific feature type and id

## Usage

``` r
osmdata_id_query(type, id)
```

## Arguments

- type:

  A character string with the OSM element type

- id:

  A character or numeric vector of length 1 with the OSM element id

## Value

An
[`osmdata::osmdata`](https://docs.ropensci.org/osmdata/reference/osmdata.html)
object with the retrieved OpenStreetMap data
