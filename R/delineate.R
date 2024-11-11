#' Delineate a corridor around a river.
#'
#' @param city_name A place name as a string
#' @param river_name A river name as a string
#' @param crs The projected coordinate reference system (CRS) to use (default
#'            to the suitable Universal Transverse Mercator (UTM) CRS)
#' @param segments Whether to carry out the corridor segmentation
#' @param riverspace Whether to carry out the riverspace delineation
#'
#' @return A simple feature geometry
#' @export
delineate_corridor <- function(city_name, river_name, crs = NULL,
                               segments = FALSE, riverspace = FALSE) {

  osmdata <- CRiSp::get_osmdata(city_name, river_name, crs)

  ...

  corridor <- CRiSp::corridor()
  if (segments) delineate_segments()
  if (riverspace) delineate_riverspace()
  return(corridor)
}

delineate_segments <- function() {
  stop("Segmentation not yet implemented.")
}

delineate_riverspace <- function() {
  stop("Riverspace delineation not yet implemented.")
}