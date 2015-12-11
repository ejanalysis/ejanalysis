#' @title Get tract, county, state, region info from US Census block group FIPS codes
#' @description
#'   Use US Census block group FIPS codes to get more information about each block group's overall location, such as State and County name.
#'   It does not check to see if the codes are valid other than counting how many characters each has.
#' @param fips Vector of US Census block group FIPS codes (missing leading zeroes are added).
#' @return Returns a data.frame with these fields: c('FIPS', 'FIPS.TRACT', 'FIPS.COUNTY', 'FIPS.ST', 'ST', 'countyname', 'statename', 'REGION') 
#'   where FIPS is the input fips, the next few are the first few characters of fips corresponding to tract, county, or state, ST is the 2-letter state abbreviation, statename is state name, countyname is county name, and REGION is USEPA Region 1-10.
#' @examples
#'  x=c("391670211002", "060730185143", "261079609003", 02, 02610, "400353734002", "371190030121", "250235022001", "550439609001", "060730170302")
#'  get.fips.etc(x)
#' @export
get.fips.etc <- function(fips) {
  ##########################################################################################################
  # Make partial FIPS fields and county and ST and REGION info
  # input should be a vector of block group fips codes 
  ##########################################################################################################
  fips <- clean.fips(fips)
  x <- data.frame(FIPS=fips,
                  FIPS.TRACT=ejanalysis::get.fips.tract(fips),
                  FIPS.COUNTY=ejanalysis::get.fips.county(fips),
                  FIPS.ST=ejanalysis::get.fips.st(fips),
                  stringsAsFactors=FALSE)
  x$countyname <- ejanalysis::get.county.info(query=x$FIPS.COUNTY, fields='fullname')$fullname
  stateinfo <- ejanalysis::get.state.info(query=x$FIPS.ST, fields=c('ST', 'statename'))
  x$ST <- stateinfo$ST
  x$statename <- stateinfo$statename
  x$REGION <- ejanalysis::get.epa.region(x$ST)
  x <- analyze.stuff::put.first(x, c('FIPS', 'FIPS.TRACT', 'FIPS.COUNTY', 'FIPS.ST', 'countyname', 'ST', 'statename', 'REGION'))
  return(x)
}
