#' @title Clean up US Census FIPS (Add Missing Leading Zeroes)
#'
#' @description Clean up US Census FIPS (add any missing leading zeroes)
#' @details  For information on FIPS codes, see \url{http://www.census.gov/geo/reference/ansi.html},
#'   and also see \url{https://www.census.gov/geo/reference/geoidentifiers.html} \cr\cr
#' If FIPS provided is 1-2 digits long assume it is a State.\cr
#' If FIPS provided is 3 digits long, it is a mistake and return NA.\cr
#' If FIPS provided is 4-5 digits, assume it is a County.\cr
#' If FIPS provided is 6-9 digits, it is a mistake and return NA.\cr
#' If FIPS provided is 10 digits long, assume it is a tract missing a leading zero on the state portion (should have 11 characters).\cr
#' If FIPS provided is 11 digits long, assume it is a tract (correctly 11 characters), not simply a block group FIPS missing a leading zero (block group FIPS would correctly would have 12 characters).\cr
#' If FIPS provided is 12 digits long, assume it is a block group (correctly 12 characters).\cr
#' If FIPS provided is 13 digits long, it is a mistake and return NA.\cr
#' If FIPS provided is 14 OR 15 digits long, assume it is a block.
#' @param FIPS Vector of numeric or character class, required. Can be state FIPs as number or character, for example.
#' @return Returns vector of FIPS (all characters from 2-digit State code onwards as appropriate) as character with leading zeroes
#' @examples  #(No example yet)
#'  @template seealsoFIPS
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

  # Remove if State code is invalid. Not checking other parts of FIPS here, however.
  # can replace this with info from a standard source at some point
  state.fips.set <- c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L,
                      18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                      31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L,
                      45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L,
                      56L, 60L, 66L, 69L, 72L, 74L, 78L)
  # this list  includes PR and island areas:  56L, 60L, 66L, 69L, 72L, 74L, 78L
  state.fips.set <- analyze.stuff::lead.zeroes(state.fips.set, 2)

  FIPS[!(substr(FIPS, 1, 2) %in% state.fips.set)] <- NA

  return(FIPS)
}
