#' @title Get State FIPS from block or block group FIPS
#'
#' @description Extract partial FIPS code from longer FIPS code
#' @details Each fips passed to the function is a FIPS code (see \url{http://www.census.gov/geo/reference/ansi.html}.)
#' @param fips Vector of one or more elements, character class, 12 or 15 characters long (block group or block), required.
#' @return Returns a vector of one or more character elements, same lengths as fips
#' @template seealsoFIPS
#' @examples
#' samplefips <- c("011030001003", "011030001003001", 11030001003001, 35, 1, NA, 'invalidtext', '02')
#' get.fips.st(samplefips)
#' @export
get.fips.st <- function(fips) {
  fips <- clean.fips.1215(fips) # checks length, NAs, infers missing leading zero, warns
  return(as.character(substr(fips, 1, 2)))
}
