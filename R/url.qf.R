#' @title US Census Quickfacts Webpage URL
#'
#' @description Get URL for webpage that provides basic demographic information from United States Census Bureau.
#' @details  For information on FIPS codes, see \url{http://www.census.gov/geo/reference/ansi.html},
#'   and also see \url{https://www.census.gov/geo/reference/geoidentifiers.html} \cr\cr
#'   	#################### \cr\cr
#' If FIPS provided is 10 digits long, assume it is a tract missing a leading zero on the state portion (should have 11 characters).\cr
#' If FIPS provided is 11 digits long, assume it is a tract (correctly 11 characters), not simply a block group FIPS missing a leading zero (block group FIPS would correctly would have 12 characters).\cr
#' If FIPS provided is 12 digits long, assume it is a block group (correctly 12 characters).\cr
#' If FIPS provided is 13 digits long, it is a block group.\cr
#' If FIPS provided is 14 OR 15 digits long, assume it is a block. But that will not work in this function. \cr
#' If FIPS is none of the above, return a default URL \cr\cr
#'
#' # NOTES:\cr
#' #\cr
#' # URL FORMATS FOR QUICKFACTS REPORT ON A COUNTY FROM CENSUS QUICKFACTS SITE: \cr
#' #\cr
#' #A WHOLE STATE: \cr
#' # \url{http://quickfacts.census.gov/qfd/states/01000.html} \cr
#' # \cr
#' #WHERE 01000 = STATE 2-DIGIT FIPS,  PLUS 3 ZEROES \cr
#' # \cr
#' #A SINGLE COUNTY: \cr
#' # e.g., url{http://quickfacts.census.gov/qfd/states/01/01005.html} \cr
#' # \cr
#' #URL = \cr
#' # http://quickfacts.census.gov/qfd/states/XX/YYYYY.html \cr
#' # \cr
#' #WHERE XX =	STATE 2-DIGIT FIPS \cr
#' #WHERE YYYYY = 	STATE 2-DIGIT FIPS AND COUNTY 3-DIGIT FIPS = 5 DIGITS TOTAL
#' @param fips Vector of numeric or character class, required. Can be state FIPs as number or character, for example.
#' @param launch TRUE by default, whether to open page in web browser (max=1st 3 URLs opened)
#' @param clean Does not use clean.fips() if FALSE, which helps if the countiesall or other list is not yet updated, for example and lacks some new FIPS code
#' @return Returns table of FIPS, geographic scale, and URL
#' @examples
#'  url.qf( c( '011030001003001', '011030001003', '01103000100', '01005', 1,
#'    c(8:10), 99999) )
#'  \dontrun{
#'  url.qf( c( '011030001003001', '011030001003', '01103000100', '01005', 1,
#'    ejanalysis::get.state.info()[ , 'FIPS.ST'], 99999) )
#'  }
#' @template seealsoFIPS
#' @export
url.qf <- function(fips="", launch=TRUE, clean=TRUE) {

  if (clean) {FIPS <- clean.fips(fips)} # checks length, NAs, infers missing leading zero, warns

  geo <- rep("bad FIPS", length(FIPS))
  geo[nchar(FIPS) == 2]	<- "state"
  geo[is.na(FIPS)]		<- "bad FIPS"
  geo[nchar(FIPS) == 5]	<- "county"
  geo[nchar(FIPS) == 11] <- "tract"
  geo[nchar(FIPS) == 12] <- "block group"
  geo[nchar(FIPS) == 15] <- "block"

  #	CREATE URLS

  defaulturl <- 'http://www.census.gov/quickfacts'
  # possible defaults:
  # 'http://www.census.gov/quickfacts'
  # 'http://quickfacts.census.gov'
  # "http://quickfacts.census.gov/qfd/states/00000.html"
  url <- rep(defaulturl, length(FIPS))

  url[geo=="bad FIPS"]	<-     defaulturl
  url[geo=="state"]		<- paste("http://quickfacts.census.gov/qfd/states/", FIPS[geo=="state"], "000.html", sep="")
  url[geo=="county"]	<- paste("http://quickfacts.census.gov/qfd/states/", substr(FIPS[geo=="county"], 1, 2), "/",  FIPS[geo=="county"], ".html", sep="")

  # Just provide info on county for tract, block group, or block since QF does not have those scales.
  others <- geo %in% c("tract", "block group", "block")
  url[others] <- paste("http://quickfacts.census.gov/qfd/states/", substr(FIPS[others], 1, 2), "/",  substr(FIPS[others], 1, 5), ".html", sep="")

  if (launch) {
    for (i in 1:(min(3, length(url)))) {
      url.open(url[i])
    }
  }

  return(data.frame(FIPS=FIPS, geo=geo, url=url, stringsAsFactors=FALSE))
}
