#' @title Identify EPA Region for given state (or FIPS)
#'
#' @description Identify US Environmental Protection Agency region(s) containing given state(s) (or via FIPS)
#' @details For EPA Regions, see <http://www2.epa.gov/aboutepa#pane-4> or <http://www2.epa.gov/aboutepa/visiting-regional-office>.
#'   For information on FIPS codes, see <http://www.census.gov/geo/reference/ansi.html>,
#'   and also see <https://www.census.gov/geo/reference/geoidentifiers.html>
#' @param state Vector of numeric or character class, required. Can be state FIPs as number or character, 2-character state abbreviation, or full state name
#' @return Returns a vector of numbers, same length as state
#' @examples
#'  myregions <- get.epa.region() # to see full list
#'  myregions <- get.epa.region('DC') # for one state by 2-letter abbreviation
#'  myregions <- get.epa.region(c('AK', 'NY', 'PR')) # for a vector of 2-letter abbreviations
#'  myregions <- get.epa.region(c('alaska', 'new york', 'puerto rico')) # vector of state names
#'  myregions <- get.epa.region(c('04', '36', '72')) # FIPS codes, character with leading zero
#'  myregions <- get.epa.region(c(4,36,72)) # state FIPS codes, numeric with no leading zeroes
#'  myregions <- get.epa.region(c('NY', 'Ohio')) # CANNOT MIX WAYS TO DEFINE STATES- PICK 1 FORMAT
#'  @seealso [clean.fips()]
#' @export
get.epa.region <- function(state) {

  if (missing(state)) {
    return(get.state.info()[ , c('ST', 'statename', 'ST', 'REGION')])
    #stop('Missing state name(s), FIPS#(s), or abbreviation(s)\n')
  }


  return( get.state.info(state)$REGION )

}


