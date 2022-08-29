#' Add col for each component of longer FIPS
#'
#'  Given a data.frame with FIPS col that is the full state county tract blockgroup FIPS
#'  returns the data.frame with extra columns up front, with components of FIPS.
#' @param bg Data.frame with a character column called FIPS (or specified otherwise)
#' @param fipscolname FIPS by default but could specify some other name to be found as col in bg
#' @param clean Does not use clean.fips() if FALSE, which helps if the countiesall or other list is not yet updated, for example and lacks some new FIPS code
#'
#' @return Returns the whole data.frame with new columns in front:
#'   'FIPS', 'FIPS.TRACT', 'FIPS.COUNTY', 'FIPS.ST', 'ST', 'statename', 'REGION'
#' @export
#'
addFIPScomponents <- function(bg, fipscolname="FIPS", clean=TRUE) {

  bgfips <- bg[ , fipscolname]

  ########################################################################################################## #
  # Add FIPS for tract, county, state
  # Add countyname, statename, State abbreviation
  # Add EPA Region number
  ########################################################################################################## #

  geotype1 <- function(fips) {
    # assuming that all of the input fips are the same type/ same scale (level of resolution)
    # figure out what resolution these are at
    if (length(unique(nchar(fips))) != 1) {
      # not all the same length, which is only OK if it is the REGION number that can be 1-10, with no leading zeroes
      if (setequal(unique(nchar(fips)), c(1, 2))) {
        # seems to be REGION field since some are 1 digit and others 2 digits long (ie REGION is 10)
        digits <- 999
      } else {
        stop('Problem with FIPS not all the same number of characters')
      }
    } else {
      # all have the same number of characters (digits)
      digits <- nchar(fips[1])
    }
    digits <- paste('n', digits, sep = '')
    bestres <- switch(
      digits,
      'n999' = 'region',
      'n2' = 'state',
      'n5' = 'county',
      'n11' = 'tract',
      'n12' = 'bg',
      'other'
    )
    return(bestres)
  }
  bestres <- geotype1(bgfips)

  if (bestres == 'other') {
    warning('Problem with FIPS - does not seem to be valid ')
  }

  # add only the relevant, less detailed scales of FIPS and related info

  if (bestres == 'state') {
    bg$REGION  <- ejanalysis::get.epa.region(bgfips)
  }
  if (bestres == 'county') {
    bg$FIPS.ST <- ejanalysis::get.fips.st(bgfips, clean=clean)
    stateinfo     <-
      ejanalysis::get.state.info(query = bg$FIPS.ST,
                                 fields = c('ST', 'statename'))
    bg$ST <- stateinfo$ST
    bg$statename <- stateinfo$statename

    bg$REGION  <-
      ejanalysis::get.epa.region(ejanalysis::get.fips.st(bgfips, clean=clean))
  }
  if (bestres == 'tract') {
    bg$FIPS.COUNTY <- ejanalysis::get.fips.county(bgfips, clean=clean)
    bg$countyname <-
      ejanalysis::get.county.info(query = bg$FIPS.COUNTY, fields = 'fullname')$fullname

    bg$FIPS.ST <- ejanalysis::get.fips.st(bgfips, clean=clean)
    stateinfo     <-
      ejanalysis::get.state.info(query = bg$FIPS.ST,
                                 fields = c('ST', 'statename'))
    bg$ST <- stateinfo$ST
    bg$statename <- stateinfo$statename

    bg$REGION  <-
      ejanalysis::get.epa.region(ejanalysis::get.fips.st(bgfips, clean=clean))
  }
  if (bestres == 'bg') {
    bg$FIPS.TRACT  <- ejanalysis::get.fips.tract(bgfips, clean=clean)  # clean.fips(bgfips[3945:3960])
    bg$FIPS.COUNTY <- ejanalysis::get.fips.county(bgfips, clean=clean)
    bg$countyname <-
      ejanalysis::get.county.info(query = bg$FIPS.COUNTY, fields = 'fullname')$fullname

    bg$FIPS.ST <- ejanalysis::get.fips.st(bgfips, clean=clean)
    stateinfo     <-
      ejanalysis::get.state.info(query = bg$FIPS.ST,
                                 fields = c('ST', 'statename'))
    bg$ST <- stateinfo$ST
    bg$statename <- stateinfo$statename

    bg$REGION  <-
      ejanalysis::get.epa.region(ejanalysis::get.fips.st(bgfips, clean=clean))
  }

  # put the most basic fields (columns) first -------------------------------
  placefields <-
    c(fipscolname,
      'FIPS.TRACT',
      'FIPS.COUNTY',
      'FIPS.ST',
      'ST',
      'statename',
      'REGION')
  placefields <- placefields[placefields %in% names(bg)]
  bg <- analyze.stuff::put.first(bg, placefields)
  return(bg)
}
