#' @title Clean up US Census FIPS (Add Missing Leading Zeroes)
#'
#' @description Clean up US Census FIPS (add any missing leading zeroes)
#' @details  For information on FIPS codes, see <http://www.census.gov/geo/reference/ansi.html>,
#'   and also see <https://www.census.gov/geo/reference/geoidentifiers.html> \cr\cr
#'  If FIPS provided is 1-2 digits long assume it is a State.\cr
#'  If FIPS provided is 3 digits long, it is a mistake and return NA.\cr
#'  If FIPS provided is 4-5 digits, assume it is a County.\cr
#'  If FIPS provided is 6-9 digits, it is a mistake and return NA.\cr
#'  If FIPS provided is 10 digits long, assume it is a tract missing a leading zero on the state portion (should have 11 characters).\cr
#'  If FIPS provided is 11 digits long, assume it is a tract (correctly 11 characters), not simply a block group FIPS missing a leading zero (block group FIPS would correctly would have 12 characters).\cr
#'  If FIPS provided is 12 digits long, assume it is a block group (correctly 12 characters).\cr
#'  If FIPS provided is 13 digits long, it is a mistake and return NA.\cr
#'  If FIPS provided is 14 OR 15 digits long, assume it is a block.
#' @param fips Vector of numeric or character class, required. Can be state FIPs as number or character, for example.
#' @return Returns vector of FIPS (all characters from 2-digit State code onwards as appropriate) as character with leading zeroes
#' @template seealsoFIPS
#' @export
clean.fips <- function(fips) {

  #	TRY TO CLEAN UP FIPS AND INFER GEOGRAPHIC SCALE

  FIPS <- fips
  FIPS <- gsub(" ", "", as.character(FIPS))

  # bad
  FIPS[nchar(FIPS) == 0]	<- NA

  # state
  FIPS[nchar(FIPS) == 1]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS) == 1], 2)
  #FIPS[nchar(FIPS) == 2]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS) == 2], 2)

  # bad
  FIPS[nchar(FIPS) == 3]	<- NA

  # county
  FIPS[nchar(FIPS) == 4]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==4], 5)
  #FIPS[nchar(FIPS) == 5]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==5], 5)

  # bad
  FIPS[nchar(FIPS) == 6]	<- NA
  FIPS[nchar(FIPS) == 7]	<- NA
  FIPS[nchar(FIPS) == 8]	<- NA
  FIPS[nchar(FIPS) == 9]	<- NA

  # tract
  FIPS[nchar(FIPS) == 10]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==10], 11)
  #FIPS[nchar(FIPS) == 11]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==11], 11)


  # block group
  #FIPS[nchar(FIPS) == 12]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==12], 12)

  # bad
  FIPS[nchar(FIPS) == 13]	<- NA

  # block
  FIPS[nchar(FIPS) == 14]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==14], 15)
  #FIPS[nchar(FIPS) == 15]	<- analyze.stuff::lead.zeroes(FIPS[nchar(FIPS)==15], 15)

  # bad
  FIPS[nchar(FIPS) > 15]	<- NA

  # Remove if State code is invalid.
  #   info from ejanalysis::get.state.info() or data(lookup.states, package='proxistat')
  # this list  includes PR and island areas:  56L, 60L, 66L, 69L, 72L, 74L, 78L
  state.fips.set <- ejanalysis::get.state.info()$FIPS.ST
  state.fips.set <- state.fips.set[!is.na(state.fips.set)]
  stateportioninvalid <- !(substr(FIPS, 1, 2) %in% state.fips.set)
  FIPS[stateportioninvalid] <- NA
  if (any(stateportioninvalid)) {warning('Some fips had invalid 2-digit state code (or NA) after any missing zeroes added')}

  # set to NA if 5+digits and the county fips portion is invalid (but get.county.info has to get updated when new counties get new fips)
  countyfipsall <- get.county.info()$FIPS.COUNTY
  countyportioninvalid <- (nchar(FIPS) >= 5) & !(substr(FIPS, 1, 5) %in% countyfipsall)
  FIPS[countyportioninvalid]  <- NA
  if (any(countyportioninvalid)) {warning('Some fips had invalid 5-digit state/county code after any missing zeroes added')}

  return(FIPS)
}
