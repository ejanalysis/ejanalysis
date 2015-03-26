#' @title Get County FIPS from longer FIPS
#'
#' @description Extract partial FIPS code from longer FIPS code
#' @details Each fips passed to the function is a FIPS code (see \url{http://www.census.gov/geo/reference/ansi.html}.)
#' @param fips Vector of one or more elements, character class, 12 or 15 characters long (block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips
#' @examples
#' get.fips.st("011030001003")
#' get.fips.st("011030001003001")
#' @export
get.fips.county <- function(fips) {
  
  # FUNCTION to get the partial FIPS character strings from a full FIPS as character strings
  
  # Check here for length of fips 
  # (could try to use lead.zeroes() if wrong because a leading zero was dropped?)
  if (!(all(nchar(fips)==12) || all(nchar(fips)==15)) ) {
    stop('input fips must be vector of character strings each 12 or 15 characters long (full FIPS through block group or block)')
  }
  
  # return the required FIPS
  return(as.character(substr(fips, 1, 5)))
}