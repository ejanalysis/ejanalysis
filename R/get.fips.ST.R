#' @title Get State FIPS from block or block group FIPS
#'
#' @description Extract partial FIPS code from longer FIPS code
#' @details For information on FIPS codes, see \url{http://www.census.gov/geo/reference/ansi.html},
#'   and also see \url{https://www.census.gov/geo/reference/geoidentifiers.html}
#'   It does not check to see if the codes are valid other than counting how many characters each has.
#' @param fips Vector of one or more elements, character class, 12 or 15 characters long (block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips
#' @template seealsoFIPS
#' @examples
#' samplefips <- c("011030001003", "011030001003001", 02610, 11030001003001, 35, 1, NA, 'invalidtext', '02')
#' get.fips.st(samplefips)
#' @export
get.fips.st <- function(fips) {
  fips <- clean.fips(fips) # checks length, NAs, infers missing leading zero, warns
  fips[nchar(fips) < 2] <- NA
  return(as.character(substr(fips, 1, 2)))
}
