#' @title Get Census block group FIPS from block or block group FIPS
#'
#' @description Extract partial FIPS code from longer FIPS code
#' @details For information on FIPS codes, see <http://www.census.gov/geo/reference/ansi.html>,
#'   and also see <https://www.census.gov/geo/reference/geoidentifiers.html>
#'   It does not check to see if the codes are valid other than counting how many characters each has.
#' @param fips Vector of one or more elements, character class, 12 or 15 characters long FIPS (block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips
#' @seealso [clean.fips()]
#' @examples
#' samplefips <- c("011030001003", "011030001003001", 11030001003001, 35, 1, NA, 'invalidtext', '02')
#' get.fips.bg(samplefips)
#' @export
get.fips.bg <- function(fips) {
  fips <- clean.fips1215(fips) # checks length, NAs, infers missing leading zero, warns
  return(as.character(substr(fips, 1, 12)))
}
