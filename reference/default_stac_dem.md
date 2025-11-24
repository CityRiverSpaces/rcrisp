# Default STAC collection

Endpoint and collection ID of the default STAC collection where to
access digital elevation model (DEM) data. This is the global Copernicus
DEM 30 dataset hosted on AWS, as listed in the EarthSearch STAC API
endpoint. Note that AWS credentials need to be set up in order to access
the data (not the catalog). References:

- [EarthSearch STAC API](https://element84.com/earth-search/)

- [Copernicus
  DEM](https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM)

- [AWS Copernicus DEM
  datasets](https://copernicus-dem-30m.s3.amazonaws.com/readme.html)

- [Data
  license](https://docs.sentinel-hub.com/api/latest/static/files/data/dem/resources/license/License-COPDEM-30.pdf)

## Usage

``` r
default_stac_dem
```

## Format

An object of class `list` of length 2.
