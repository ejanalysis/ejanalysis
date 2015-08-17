#' @title Infer Type of Data as FIPS / ANSI Codes or Names of U.S. Geographies
#' @description
#'   Tries to interpret vector of one or more FIPS codes and/or names of geographies.
#' @param x Required vector of one or more numeric or character FIPS and/or names of geographic locations.
#'   Allowed types are State, County (or equivalent), tract, block group, and block.
#'   Names for tracts, blockgroups, and blocks are not provided or interpreted.
#'   FIPS codes here are all the relevant digits starting with the 2-character state FIPS,
#'   so county fips must be 4-5 digits or characters for example (leading zeroes are inferred where
#'   possible and included in outputs). See \code{\link{clean.fips}} for details.
#' @return ***TBD *** Returns *** types as character strings, and maybe cleaned values themselves? ***
#' @seealso \code{\link{geofips}}, \code{\link[ejanalysis]{get.fips.st}} and related functions noted there, \code{\link[ejanalysis]{clean.fips}}, \code{\link[ejanalysis]{get.state.info}}
#' @examples # none yet
#' @export
geotype <- function(x) {
  stop('not done yet')

  # figure out what type of geo these are

  # BUT DO I CLEAN THEM UP AND RETURN CLEANED VERSIONS,
  # AND/OR
  # FIGURE OUT THE TYPE HERE?
  #
  # WHAT FUNCTION DOES WHICH OF THOSE?? SENSIBLE TO DO BOTH AT ONCE?
  # PRECLEAN FIRST,
  # THEN INFER TYPE BY FORM,
  # THEN VALIDATE TYPE AGAINST LIST OF VALIDS OF THAT TYPE,
  # THEN
  #   1-RETURN CLEANED AND VALIDATED VERSION &
  #   2-RETURN NAME OF THE TYPE.

  # What type of fips or name was provided if any?


  # from clean.fips():
  #   If FIPS provided is 1-2 digits long assume it is a State.
  #   If FIPS provided is 3 digits long, it is a mistake and return NA.
  #   If FIPS provided is 4-5 digits, assume it is a County.
  #   If FIPS provided is 6-9 digits, it is a mistake and return NA.
  #   If FIPS provided is 10 digits long, assume it is a tract missing a leading zero on the state portion (should have 11 characters).
  #   If FIPS provided is 11 digits long, assume it is a tract (correctly 11 characters), not simply a block group FIPS missing a leading zero (block group FIPS would correctly would have 12 characters).
  #   If FIPS provided is 12 digits long, assume it is a block group (correctly 12 characters).
  #   If FIPS provided is 13 digits long, it is a mistake and return NA.
  #   If FIPS provided is 14 OR 15 digits long, assume it is a block.

  if (1==0) {

    fips <- as.character(fips)
    if (nchar(fips)==1) {fips <- lead.zeroes(fips, 2)}
    if (nchar(fips)==4) {fips <- lead.zeroes(fips, 5)}

    if (nchar(fips)==2 && (fips %in% valid.FIPS.ST$FIPS.ST)) {fipstype <- 'fips.state'} else {
      if (nchar(fips)==5 && (fips %in% valid.FIPS.COUNTY)) {fipstype <- 'fips.county'} else {
        if (tolower(fips) %in% valid.FIPS.ST$statename) {fipstype <- 'name.state'} else {

          # TRY TO FIGURE OUT COUNTY FROM COUNTYNAME BUT NEED STATE ALSO AND NOT YET IMPLEMENTED:

          # obtain countyname list here to check that, since it isn't any of the other types checked so far ****** UNTESTED - NOT WORKING YET
          countyinfo <- get.county.info()
          countyportion <- gsub(', [[:alnum:]_]+', '', tolower(fips))
          stateportion <-  gsub('^.+, ', '', tolower(fips))
          stateok <- FALSE
          if (stateportion %in% valid.FIPS.ST$statename) { stateok <- TRUE }
          if (stateportion %in% tolower(valid.FIPS.ST$ST)) { stateportion <- valid.FIPS.ST$statename[match(tolower(stateportion), tolower(valid.FIPS.ST$ST))]; stateok <- TRUE}
          cat(stateportion, countyportion, stateok,'\n')
          if ((countyportion %in% tolower(countyinfo$countyname))  &  stateok  ) { fips <- countyinfo$FIPS.COUNTY[match(tolower(fips), tolower(countyinfo$countyname))]; fipstype <- 'name.county'} else {
            #if ( charmatch(tolower(fips), ....  xxx ) {warning(paste('only partial match on countyname - not used: ', partmatch, sep='') )}
            fipstype='invalid'
          }
        }
      }
    }

######## another version / start

    valid.states <- ejanalysis::get.state.info()[ , c('FIPS.ST', 'statename', 'ST')]
    #   ejanalysis::get.state.info   uses data(lookup.states, package='proxistat')
    valid.states$statename <- tolower(valid.states$statename)

    valid.counties  <- ejanalysis::get.county.info()
    # that uses  data(countiesall, package='proxistat') # has "ST", "countyname", "FIPS.COUNTY", "statename", "fullname"

    # What type of fips or name was provided if any?

    if (missing(x)) {warning('No x specified'); return(NA)}

    nx <- length(x)
    x <- as.character(x)
    type <- rep('invalid', nx) # default is 'invalid'

    # where x is 1 character, assume it was a state fips that dropped the leading zero
    x[nchar(x)==1] <- lead.zeroes(x[nchar(x)==1], 2)

    # where fips represents a 4 digit number, not name like Ohio, add the missing leading zero to get county fips
    fips[nchar(fips)==4 & suppressWarnings(!is.na(as.numeric(fips)))] <- lead.zeroes(fips[nchar(fips)==4 & suppressWarnings(!is.na(as.numeric(fips)))], 5)

    fipstype[nchar(fips)==2 & (fips %in% valid.states$FIPS.ST)] <- 'fips.state'
    fipstype[nchar(fips)==2 & (fips %in% valid.states$ST)] <- 'abbrev.state'
    fipstype[tolower(fips) %in% valid.states$statename] <- 'name.state'

    fips[nchar(fips)==4 & (analyze.stuff::lead.zeroes(fips, 5) %in% valid.counties$FIPS.COUNTY)] <- analyze.stuff::lead.zeroes(fips, 5)
    fipstype[nchar(fips)==5 & (fips %in% valid.counties$FIPS.COUNTY)] <- 'fips.county'

    #print('fipstypes');print(fipstype);print("")

    # if (1==0) {

    # WILL TRY TO FIGURE OUT COUNTY FROM COUNTYNAME BUT NEED STATE ALSO AND NOT YET IMPLEMENTED:
    # obtain countyname list here to check that, since it isn't any of the other types checked so far
    #  ****** UNTESTED - NOT WORKING YET

    countyportion <- gsub(', [[:alnum:]_]+', '', tolower(fips))
    stateportion <-  gsub('^.+, ', '', tolower(fips))

    STportion <- valid.states$FIPS.ST[match(tolower(stateportion), tolower(valid.states$ST))]
    # convert state abbreviation to full state name
    stateportion[stateportion %in% valid.states$FIPS.ST] <- valid.states$statename[match(tolower(stateportion[stateportion %in% valid.states$FIPS.ST]), tolower(valid.states$FIPS.ST))]

    stateok <- rep(FALSE, length(fips) )
    stateok[stateportion %in% valid.states$statename]  <- TRUE
    stateok[STportion %in% valid.states$FIPS.ST]  <- TRUE
    #cat(stateportion, countyportion, stateok,'\n')
    countyok <- (tolower(countyportion) %in% tolower(valid.counties$countyname))

    fips[countyok  &  stateok] <- valid.counties$FIPS.COUNTY[match(
      paste(tolower(countyportion[countyok  &  stateok]), tolower(stateportion[countyok  &  stateok]), sep = ''),
      paste(tolower(valid.counties$countyname), tolower(valid.counties$statename), sep = '')
    )]
    fipstype[countyok  &  stateok] <- 'name.county'
    #fipstype[countyok  &  stateok] <- 'fips.county'

    #if ( charmatch(tolower(fips), ....  xxx ) {warning(paste('only partial match on countyname - not used: ', partmatch, sep='') )}
    #  }

    if (all(fipstype=='invalid')) {
      stop('no valid fips')
    }

    if (any(fipstype=='invalid')) {
      warning('some invalid fips - returning USA URL for those')
      fipstype[fipstype=='invalid'] <- 'usa'
    }

    return(type)
  }

  }
