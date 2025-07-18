% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exampledata.R
\name{get_osm_example_data}
\alias{get_osm_example_data}
\title{Get example OSM data}
\usage{
get_osm_example_data()
}
\value{
A list of sf objects containing the OSM data.
}
\description{
This function retrieves example OpenStreetMap (OSM) data from a persistent
URL on the 4TU.ResearchData data repository, and it can be used in examples
and tests. The code used to generate the example dataset is available at
https://github.com/CityRiverSpaces/CRiSpExampleData
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
get_osm_example_data()
\dontshow{\}) # examplesIf}
}
