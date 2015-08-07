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
#' If FIPS provided is 14 OR 15 digits long, assume it is a block. But that will not work in this function. \cr\cr
#' # NOTES:
#' #
#' # URL FORMATS FOR QUICKFACTS REPORT ON A COUNTY FROM CENSUS QUICKFACTS SITE:
#' #
#' #A WHOLE STATE:
#' #http://quickfacts.census.gov/qfd/states/01000.html
#' #
#' #WHERE 01000 = STATE 2-DIGIT FIPS,  PLUS 3 ZEROES
#' #
#' #A SINGLE COUNTY:
#' #http://quickfacts.census.gov/qfd/states/01/01001.html
#' #
#' #URL =
#' #http://quickfacts.census.gov/qfd/states/XX/YYYYY.html
#' #
#' #WHERE XX =	STATE 2-DIGIT FIPS
#' #WHERE YYYYY = 	STATE 2-DIGIT FIPS AND COUNTY 3-DIGIT FIPS = 5 DIGITS TOTAL
#' @param FIPS Vector of numeric or character class, required. Can be state FIPs as number or character, for example.
#' @return Returns table of FIPS, geographic scale, and URL
#' @examples  #(No example yet)
#'  @template seealsoFIPS
#' @export
url.qf <- function(fips="") {

  FIPS <- clean.fips(fips)

  url <- rep("", length(FIPS))

  geo <- rep("county", length(FIPS))
  geo[is.na(FIPS)]		<- "bad FIPS"
  geo[nchar(FIPS) == 2]	<- "state"
  geo[nchar(FIPS) == 5]	<- "county"
  geo[nchar(FIPS) == 11] <- "tract"
  geo[nchar(FIPS) == 12] <- "block group"
  geo[nchar(FIPS) == 15] <- "block"

  #	CREATE URLS

  url[geo=="bad FIPS"]	<- "http://quickfacts.census.gov/qfd/states/00000.html" # USA default
  url[geo=="state"]		<- paste("http://quickfacts.census.gov/qfd/states/", FIPS, "000.html", sep="")
  url[geo=="county"]	<- paste("http://quickfacts.census.gov/qfd/states/", substr(FIPS, 1, 2), "/",  FIPS, ".html", sep="")
  url[geo %in% c("tract", "block group", "block")] <- paste("http://quickfacts.census.gov/qfd/states/", substr(FIPS, 1, 2), "/",  FIPS, ".html", sep="")

  return(data.frame(FIPS=FIPS, geo=geo, url=url, stringsAsFactors=FALSE))
}
