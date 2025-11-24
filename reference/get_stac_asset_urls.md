# Retrieve the URLs of all the assets intersecting a bbox from a STAC API

Retrieve the URLs of all the assets intersecting a bbox from a STAC API

## Usage

``` r
get_stac_asset_urls(bb, endpoint = NULL, collection = NULL)
```

## Arguments

- bb:

  A bounding box, provided either as a matrix (rows for "x", "y",
  columns for "min", "max") or as a vector ("xmin", "ymin", "xmax",
  "ymax"), in lat/lon coordinates (WGS84 coordinate referece system) of
  class `bbox`

- endpoint:

  URL of the STAC API endpoint. To be provided together with
  `stac_collection`, or leave blank to use defaults (see
  [`default_stac_dem`](https://cityriverspaces.github.io/rcrisp/reference/default_stac_dem.md))

- collection:

  Identifier of the STAC collection to be queried. To be provided
  together with `stac_endpoint`, or leave blank to use defaults (see
  [`default_stac_dem`](https://cityriverspaces.github.io/rcrisp/reference/default_stac_dem.md))

## Value

A list of URLs for the assets in the collection overlapping with the
specified bounding box

## Examples

``` r
if (FALSE) { # interactive()
bb <- get_osm_bb("Bucharest")
get_stac_asset_urls(bb)

# Use non-default STAC API
get_stac_asset_urls(bb,
                    endpoint = "some endpoint",
                    collection = "some collection")
}
```
