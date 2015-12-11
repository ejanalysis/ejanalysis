#' @title Get County FIPS from block or block group FIPS
#'
#' @description Extract partial FIPS code from longer FIPS code
#' @details Each fips passed to the function is a FIPS code (see \url{http://www.census.gov/geo/reference/ansi.html}.)
#'   It does not check to see if the codes are valid other than counting how many characters each has.
#' @param fips Vector of one or more elements, character class, 5, 11, 12 or 15 characters long after missing leading zeroes inferred (county, tract, block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips
#'   NA where input cannot be converted to county/tract/block group/block length code, as with state fips as input.
#' @template seealsoFIPS
#' @examples
#' samplefips <- c("011030001003", "011030001003001", 02610, 11030001003001, 35, 1, NA, 'invalidtext', '02')
#' get.fips.county(samplefips)
#' @export
get.fips.county <- function(fips) {
  fips <- clean.fips(fips) # checks length, NAs, infers missing leading zero, warns
  fips[nchar(fips) < 5] <- NA
  return(as.character(substr(fips, 1, 5)))
}
