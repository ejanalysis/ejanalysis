#' @title Check and clean Census block group or block FIPS - only check number of characters and if NA, not if valid number otherwise
#'
#' @description Check if valid Census block group or block FIPS, warn if not, add missing leading zero if inferred.
#' @details For information on FIPS codes, see <http://www.census.gov/geo/reference/ansi.html>,
#'   and also see <https://www.census.gov/geo/reference/geoidentifiers.html>
#' @param fips Vector of one or more elements, ideally character class, ideally 12 or 15 characters long (block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips, NA if NA input
#' @seealso [clean.fips()]
#' @examples
#'   samplefips <- c("011030001003", "011030001003001", 11030001003001, 35, 1, NA, 'invalidtext', '02')
#'   clean.fips1215(samplefips)
#'   clean.fips1215("011030001003001")
#' @export
clean.fips1215 <- function(fips) {
  # used by get.fips.bg, get.fips.county, etc.
  if (is.numeric(fips)) {fips <- as.character(fips)}
  # Check length of fips and if NA


  fips <- analyze.stuff::lead.zeroes(fips) # now this makes all the right length by adding zeroes as needed.

  if (!(all(nchar(fips) == 12) || all(nchar(fips) == 15)) ) {
    if (!(all(nchar(fips[!is.na(fips)]) == 12) || all(nchar(fips[!is.na(fips)]) == 15)) ) {
      warning('input fips should be vector of character strings each 12 or 15 characters long (full FIPS through block group or block)')
    }
    if (any(is.na(fips))) {
      warning('NA values returned for NA values in input')
    }

    # # (try to use analyze.stuff::lead.zeroes() if wrong because a leading zero was dropped)
    # if (any(nchar(fips)==11)) {
    #   warning('added an inferred missing leading zero where fips was 11 rather than 12 characters long')
    #   fips[nchar(fips)==11] <- analyze.stuff::lead.zeroes(fips[nchar(fips)==11], 12)
    # }
    # if (any(nchar(fips)==14)) {
    #   warning('added an inferred missing leading zero where fips was 14 rather than 15 characters long')
    #   fips[nchar(fips)==14] <- analyze.stuff::lead.zeroes(fips[nchar(fips)==14], 15)
    # }

  }
  return(fips)
}
