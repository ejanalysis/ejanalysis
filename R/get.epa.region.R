#' @title Identify EPA Region for given state
#'
#' @description Identify US Environmental Protection Agency region(s) containing given state(s)
#' @details See \url{}
#' @param state Vector of numeric or character class, required. Can be state FIPs as number or character, 2-character state abbreviation, or full state name
#' @return Returns a vector of numbers, same length as state
#' @examples
#'  myregions <- get.epa.region() # to see full list
#'  myregions <- get.epa.region('DC') # for one state by 2-letter abbreviation
#'  myregions <- get.epa.region(c('AK', 'NY', 'PR')) # for a vector of 2-letter abbreviations
#'  myregions <- get.epa.region(c('alaska', 'new york', 'puerto rico')) # for a vector of state names
#'  myregions <- get.epa.region(c('04', '36', '72')) # for a vector of state FIPS codes as characters with leading zeroes
#'  myregions <- get.epa.region(c(4,36,72)) # for a vector of state FIPS codes as numeric with no leading zeroes
#'  myregions <- get.epa.region(c('NY', 'Ohio')) # CANNOT MIX WAYS OF DEFINING STATES - PICK ONE FORMAT
#'  @template seealsoFIPS
#' @export
get.epa.region <- function(state) {

  if (missing(state)) { 
    return(get.state.info()[ , c('ST', 'statename', 'ST', 'REGION')])
    #stop('Missing state name(s), FIPS#(s), or abbreviation(s)\n')
  }
	
  return( get.state.info(state)$REGION )

}


