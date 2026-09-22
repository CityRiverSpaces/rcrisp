# Package index

## Delineation

Functions for full or specific delineation

- [`delineate()`](https://cityriverspaces.github.io/rcrisp/reference/delineate.md)
  : Delineate a corridor around a river
- [`delineate_valley()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_valley.md)
  : Extract the river valley from the DEM
- [`delineate_corridor()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_corridor.md)
  : Delineate a river corridor on a spatial network
- [`delineate_segments()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_segments.md)
  : Split a river corridor into segments
- [`delineate_riverspace()`](https://cityriverspaces.github.io/rcrisp/reference/delineate_riverspace.md)
  : Delineate the space surrounding a river

## Data

### Get OSM data

Get OSM data for delineation

- [`get_osmdata()`](https://cityriverspaces.github.io/rcrisp/reference/get_osmdata.md)
  : Retrieve OpenStreetMap data for a given location
- [`get_osm_bb()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_bb.md)
  : Get the bounding box of a city
- [`get_osm_city_boundary()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_city_boundary.md)
  : Get the city boundary from OpenStreetMap
- [`get_osm_river_centerline()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_river_centerline.md)
  : Get the river centreline from OpenStreetMap
- [`get_osm_river_surface()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_river_surface.md)
  : Get the river surface from OpenStreetMap
- [`get_osm_streets()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_streets.md)
  : Get OpenStreetMap streets
- [`get_osm_railways()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_railways.md)
  : Get OpenStreetMap railways
- [`get_osm_buildings()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_buildings.md)
  : Get OpenStreetMap buildings
- [`get_river_aoi()`](https://cityriverspaces.github.io/rcrisp/reference/get_river_aoi.md)
  : Get an area of interest (AoI) around a river, cropping to the
  bounding box of a city
- [`osmdata_as_sf()`](https://cityriverspaces.github.io/rcrisp/reference/osmdata_as_sf.md)
  : Retrieve OpenStreetMap data as sf object

### Get DEM data

Get DEM data for delineation

- [`get_dem()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem.md)
  : Access digital elevation model (DEM) for a given region
- [`load_dem()`](https://cityriverspaces.github.io/rcrisp/reference/load_dem.md)
  : Retrieve DEM data from a list of STAC assets
- [`default_stac_dem`](https://cityriverspaces.github.io/rcrisp/reference/default_stac_dem.md)
  : Default STAC collection
- [`get_stac_asset_urls()`](https://cityriverspaces.github.io/rcrisp/reference/get_stac_asset_urls.md)
  : Retrieve the URLs of all the assets intersecting a bbox from a STAC
  API

### Example data

Example OSM, DEM and delineation data

- [`get_osm_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_osm_example_data.md)
  : Get example OSM data
- [`get_dem_example_data()`](https://cityriverspaces.github.io/rcrisp/reference/get_dem_example_data.md)
  : Get example DEM data
- [`bucharest_dambovita`](https://cityriverspaces.github.io/rcrisp/reference/bucharest_dambovita.md)
  : rcrisp example delineation data for Bucharest

## Network preparation

Functions to prepare network for delineation

- [`as_network()`](https://cityriverspaces.github.io/rcrisp/reference/as_network.md)
  : Create a network from a collection of line strings
- [`flatten_network()`](https://cityriverspaces.github.io/rcrisp/reference/flatten_network.md)
  : Flatten a network by adding points at apparent intersections
- [`clean_network()`](https://cityriverspaces.github.io/rcrisp/reference/clean_network.md)
  : Clean a spatial network

## Helpers

Helpers used throughout delineation and data retrieval functions

- [`as_bbox()`](https://cityriverspaces.github.io/rcrisp/reference/as_bbox.md)
  : Get the bounding box from the x object
- [`as_crs()`](https://cityriverspaces.github.io/rcrisp/reference/as_crs.md)
  : Standardise the coordinate reference system (CRS) of an object
- [`get_utm_zone()`](https://cityriverspaces.github.io/rcrisp/reference/get_utm_zone.md)
  : Get the UTM zone of a spatial object
- [`reproject()`](https://cityriverspaces.github.io/rcrisp/reference/reproject.md)
  : Reproject a raster or vector dataset to the specified coordinate
  reference system (CRS)

## Cache

Functions to handle cached data

- [`check_cache()`](https://cityriverspaces.github.io/rcrisp/reference/check_cache.md)
  : Check cache
- [`clear_cache()`](https://cityriverspaces.github.io/rcrisp/reference/clear_cache.md)
  : Remove cache files
- [`cache_directory()`](https://cityriverspaces.github.io/rcrisp/reference/cache_directory.md)
  : Return the cache directory used by the package
