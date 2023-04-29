#' @title Extract state name from Census geo name field
#' @description Parse text names of places from Census Bureau datasets like American Community Survey 5-year Summary File.
#' Extracts partial (e.g., just state or county name) text name of a place from the full Census text name of a place.
#' @param placename character vector, required, with text names of places from Census Bureau,
#'   such as 'Block Group 1, Census Tract 1, Aleutians East Borough, Alaska'
#' @details Inputs can be tracts or block groups as in the relevant ACS summary files, possibly others at some point.  \cr
#'  For tracts, there are only 3 parts to placename (tract, county, state)  \cr
#'	For block groups, there are 4 parts to placename (block group, tract, county, state) \cr
#'	Note that County names are not unique -- the same County name may exist in 2+ States. \cr
#'	also see http://www.census.gov/geo/reference/ansi.html  \cr
#'	See <http://www.census.gov/geo/reference/codes/files/national_county.txt> but file has moved
#'  Note old code was in GET COUNTY NAMES FROM NHGIS DATASET.R
#' @template seealsoFIPS
#' @return character vector of names
#' @examples
#' # Test data where some are block groups and some are tracts,
#' # as in file downloaded from Census FTP site for ACS 5-year Summary File:
#'
#' testnames <- c(
#'   'Block Group 1, Census Tract 1, Aleutians East Borough, Alaska',
#'   'Block Group 2, Census Tract 1, Aleutians East Borough, Alaska',
#'   'Block Group 1, Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Block Group 2, Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Block Group 1, Census Tract 2.01, Anchorage Municipality, Alaska',
#'   'Census Tract 1, Aleutians East Borough, Alaska',
#'   'Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Census Tract 2.01, Anchorage Municipality, Alaska')
#' testnames <- rep(testnames, floor(280000/8))
#'
#' mynames1 <- get.name.state(testnames)
#' head(mynames1, 20)
#' mynames2 <- get.name.county(testnames)
#' head(mynames2, 20)
#' @export
get.name.state  <- function(placename) {
  x <- strsplit(testnames, ", ")
  y <- unlist(x)
  y <- y[!(substr(y, 1, 11) %in% c('Block Group', 'Census Trac'))]
  y <- matrix(y, ncol=2, byrow=TRUE)
  colnames(y) <- c('name.county', 'name.state')

  return(y$name.state)
}
