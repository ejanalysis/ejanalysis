#' Get County or Tract Narrative Profile webpage from Census
#'
#'
#'  For 1 tract, URL could be for example
#'   https://www.census.gov/acs/www/data/data-tables-and-tools/narrative-profiles/2019/report.php?geotype=tract&tract=700311&state=24&county=031
#'  For 1 county, URL could be for example
#'   https://www.census.gov/acs/www/data/data-tables-and-tools/narrative-profiles/2019/report.php?geotype=county&state=21&county=017
#'
#' @param yearnumber for what year of the 1-year American Community Survey data
#' @param geotype county or tract
#' @param statenumber2 two digit state FIPS (leading zero added if necessary)
#' @param countynumber3 three digit county FIPS (leading zero added if necessary)
#' @param tractnumber6 six digit tract FIPS
#' @param launch If set to TRUE, will launch a web browser and open the narrative profile page using the url
#'
#' @return URL that is link to Census Bureau webpage with narrative profile of multiple statistics
#' @export
#'
narrativeprofile <- function(yearnumber=2020, geotype='county', statenumber2='24', countynumber3='031', tractnumber6=NULL, launch=FALSE) {

  # CENSUS NARRATIVE PROFILE WEBPAGES VIA URL ENCODED

  # slow as a loop and could recode to be more "vectorized"
  # for (i in 1:length())

  # check for obvious errors in inputs
  if (!(geotype %in% c('tract', 'county'))) {stop('geotype must be tract or county')}
  thisyr <- as.numeric(substr(Sys.Date(),1,4))
  if (any(
    yearnumber > thisyr,
    yearnumber < thisyr - 2, # just assume not allowed to go back very far
    nchar(countynumber3) < 2,
    nchar(countynumber3) > 3,
    nchar(statenumber2) < 1,
    nchar(statenumber2) > 2,
    !is.null(tractnumber6) && (nchar(tractnumber6) > 6),
    !is.null(tractnumber6) && (nchar(tractnumber6) < 5)
  )) {stop('error in input parameters like statenumber, etc.')}

  ############################################################ #
  # add leading zeroes ####
  copy_of_analyze.stuff_lead.zeroes <- function (fips, length.desired) {
    # copy of part of the older version of analyze.stuff::lead.zeroes(), where length.desired cannot be a vector
    navalues <- which(is.na(fips))
    fips <- as.character(fips)
    fips <- paste(paste(rep(rep("0", length(length.desired)),
                            length.desired), collapse = ""), fips, sep = "")
    fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))
    fips[navalues] <- NA
    return(fips)
  }
  statenumber2 <- copy_of_analyze.stuff_lead.zeroes(statenumber2, 2)
  countynumber3	<- copy_of_analyze.stuff_lead.zeroes(countynumber3, 3)
  if (!is.null(tractnumber6)) {
    tractnumber6	<- copy_of_analyze.stuff_lead.zeroes(tractnumber6, 6)
  }
  # statenumber2 <- analyze.stuff::lead.zeroes(statenumber2, 2)
  # countynumber3	<- analyze.stuff::lead.zeroes(countynumber3, 3)
  # tractnumber6	<- analyze.stuff::lead.zeroes(tractnumber6, 6)

  ############################################################ #

  myurl <- paste0(
    'https://www.census.gov/acs/www/data/data-tables-and-tools/narrative-profiles/',
    yearnumber,
    '/report.php?geotype=', geotype,
    ifelse(!is.null(tractnumber6) && geotype == 'tract', paste0('&tract=', tractnumber6), ''),
    '&state=', statenumber2,
    '&county=', countynumber3
  )
  if (launch) {browseURL(myurl)}
  # myurl <- urltools::url_encode(myurl)   # might want this?
  return(myurl)
}

