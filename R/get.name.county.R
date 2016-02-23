#' @title Extract county name from Census geo name field
#' @description Parse text names of places from Census Bureau datasets like American Community Survey 5-year Summary File.
#' Extracts partial (e.g., just state or county name) text name of a place from the full Census text name of a place.
#' @param placename character vector, required, with text names of places from Census Bureau,
#'   such as 'Block Group 1, Census Tract 1, Aleutians East Borough, Alaska'
#' @details Inputs can be tracts or block groups as in the relevant ACS summary files, possibly others at some point.  \cr
#'  For tracts, there are only 3 parts to placename (tract, county, state)  \cr
#'	For block groups, there are 4 parts to placename (block group, tract, county, state) \cr
#'	Note that County names are not unique -- the same County name may exist in 2+ States. \cr
#'	also see \url{http://www.census.gov/geo/reference/ansi.html}  \cr
#'	See \url{http://www.census.gov/geo/reference/codes/files/national_county.txt} but file has moved
#'  Note old code was in GET COUNTY NAMES FROM NHGIS DATASET.R
#' @template seealsoFIPS
#' @return character vector of names
#' @examples
#' # Test data where some are block groups and some are tracts,
#' # as in file downloaded from Census FTP site for ACS 5-year Summary File:
#'
#' testnames <- c(
#'   'Block Group 1, Census Tract 1, Aleutians East Borough, Alaska',
#'   'Block Group 2, Census Tract 1, Aleutians East Borough, Alaska',
#'   'Block Group 1, Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Block Group 2, Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Block Group 1, Census Tract 2.01, Anchorage Municipality, Alaska',
#'   'Census Tract 1, Aleutians East Borough, Alaska',
#'   'Census Tract 2, Aleutians West Census Area, Alaska',
#'   'Census Tract 2.01, Anchorage Municipality, Alaska')
#' testnames <- rep(testnames, floor(280000/8))
#'
#' mynames1 <- get.name.state(testnames)
#' head(mynames1, 20)
#' mynames2 <- get.name.county(testnames)
#' head(mynames2, 20)
#' @export
get.name.county  <- function(placename) {

  x <- strsplit(testnames, ", ")
  y <- unlist(x)
  y <- y[!(substr(y, 1, 11) %in% c('Block Group', 'Census Trac'))]
  y <- matrix(y, ncol=2, byrow=TRUE)
  colnames(y) <- c('name.county', 'name.state')

  return(y$name.county)
}


if (1==0) {
######################### when places has both bg and tracts in it:
#> length(placenames.t$FIPS)
#[1] 66322
#> length(placenames.bg$FIPS)
#[1] 211274
#>
#> table(places$sumlevel)
#
#   140    150
# 66322 211274
#> length(places$FIPS)
#[1] 277596

#############################################################################
# checking the work
#############################################################################

#	view the county names extracted, particularly those which don't have the word "County" in them, to check this
# cbind(sort(table(placenames.bg$county[!grepl("County", placenames.bg$county)])))

#	verify county name/state is unique and matches a unique county FIPS
length(placenames.bg$county)
# 211274 rows in bg list as there should be, 10/27/2011
# if it includes tracts and block groups, has 277596 as of 10/27/2011 if not limited to bg
  table(places$sumlevel)
length(unique(placenames.bg$county))	# some names apply to multiple counties so only 1955 unique names here
length(unique(paste(placenames.bg$county,placenames.bg$state,sep="")))	# can the same county name be used twice in one state??
# 3221 if just BG
length(unique(places$FIPS.COUNTY))
# 3221

#############################################################################
# JUST DOUBLE CHECK TO ENSURE A 1 TO 1 CORRESPONDENCE BETWEEN FIPS.COUNTY AND COUNTY NAME AS EXTRACTED
#############################################################################

x = aggregate(places$FIPS.COUNTY[places$sumlevel==150], by=list( paste(placenames.bg$county,placenames.bg$state,sep="")   ), function(x) length(unique(x)) )
names(x) = c("countystate", "count.unique.FIPS.COUNTY")
	# x = x[x$count.unique.FIPS.COUNTY>1,] # or just those with multiple matches, but there are none
x[order(x$count.unique.FIPS.COUNTY),] # sort by number of matches
 table(x$count.unique.FIPS.COUNTY)
#   1
#3221
# OK - EXACT 1 TO 1 MATCHES.

x = aggregate( paste(placenames.bg$county,placenames.bg$state,sep="") , by=list(places$FIPS.COUNTY[places$sumlevel==150] ), function(x) length(unique(x)) )
names(x) = c("countystate", "count.unique.NAMES")
	# x = x[x$count.unique.NAMES>1,] # or just those with multiple matches, but there are none
x[order(x$count.unique.NAMES),] # sort by number of matches
 table(x$count.unique.NAMES)
# OK - EXACT 1 TO 1 MATCHES.
 rm(x)

#############################################################################
#	NOW ADD THOSE COUNTY NAMES TO THE PLACES DATASET
#############################################################################

places$County[places$sumlevel==150] = as.character(placenames.bg$county)
places$State[places$sumlevel==150] =  as.character(placenames.bg$state)

places$County[places$sumlevel==140] = as.character(placenames.t$county)
places$State[places$sumlevel==140] =  as.character(placenames.t$state)

#	SPOT CHECK RESULTS OF THIS

places[sample(length(places$FIPS), 50), c("County", "State", "FIPS", "FIPS.COUNTY", "sumlevel")]
# OK.

cat(paste("#  ",names(places), "\n", sep=""))
cbind(table(places$State))

#	CLEAN UP
rm(placenames.bg, placenames.t)
Sgc()
}
