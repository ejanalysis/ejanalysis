#' See webpage with data on Census units from American Fact Finder - may not be used
#' @description DRAFT CODE to see webpage with table of Census Bureau data via AFF WITHOUT NEEDING API KEY
#' @details For information on FIPS codes, see <http://www.census.gov/geo/reference/ansi.html>,
#'
#'   and also see (https://www.census.gov/geo/reference/geoidentifiers.html)
#'
#'  For links to AFF, see (http://factfinder2.census.gov/files/AFF_deep_linking_guide.pdf)
#'
#'   e.g. to get 1 block census 2010 pop, for block fips 360610127001000 :
#'
#'   (http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_SF1/P1/1000000US360610127001000)
#'
#'   e.g. to get 1 block census 2010 RACE/ETH/NHWA, for block fips 360610127002001 :
#'
#'   (http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001)
#'
#'   Notice the second one gets P2 from 10_PL not 10_SF1, because P2 in SF1 means something different
#'   TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON ONE BLOCK:
#'   block fips 360610127002001
#'
#'   (http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001)
#'
#'   TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON TWO BLOCKS:
#'
#'   <http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001|1000000US360610127002000>
#' @param fips vector of FIPS
#' @param censustable 'P2' by default but can be another table code e.g., see
#'
#'   (http://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=dataset&id=dataset.en.DEC_10_SF1)
#' @param censusfile 'DEC/10_PL' by default. Also see 'DEC_10_SF1' for example.
#' @param launch TRUE by default, whether to open page in web browser
#' @param clean Does not use clean.fips1215() if FALSE, which helps if the countiesall or other list is not yet updated, for example and lacks some new FIPS code
#' @return Can open a webpage. Returns vector of URL(s) as character
#' @seealso [url.censusblock()] and  (http://ejanalysis.github.io/countyhealthrankings)
#' @examples
#'   myfips <- "360610127002001"
#'   url.census(myfips, launch=FALSE)
#'   myfips <- c("360610127002001", "360610127002000")
#'   url.census(myfips, launch=FALSE)
#' @export
url.census <- function(fips, censustable='P2', censusfile='DEC/10_PL', launch=TRUE, clean=TRUE) {

  if (clean) {fips <- clean.fips1215(fips)}
  if (any(nchar(fips)!=15)) {warning('fips must have 15 characters including leading zero if needed')}
  # or use this to allow state, county, tract, block group, block:
  #  fips <- clean.fips(fips)


  if (length(fips)==1) {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/1000000US', fips, sep='')
  } else {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/', paste( paste('1000000US',fips,sep=''), collapse='|'), sep='')
  }

  if (launch) { url.open(myurl) }
  return(myurl)
}

