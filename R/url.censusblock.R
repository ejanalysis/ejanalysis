#' @title See webpage with data on Census block(s)
#' @description DRAFT CODE TO see table webpage of BLOCK DATA VIA AFF WITHOUT NEEDING API KEY
#' @details For information on FIPS codes, see \url{http://www.census.gov/geo/reference/ansi.html},
#'   and also see \url{https://www.census.gov/geo/reference/geoidentifiers.html}. \cr\cr
#'  For links to AFF, see \url{http://factfinder2.census.gov/files/AFF_deep_linking_guide.pdf} \cr
#'   e.g. to get 1 block census 2010 pop, for block fips 360610127001000 :  \url{http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_SF1/P1/1000000US360610127001000} \cr
#'   e.g. to get 1 block census 2010 RACE/ETH/NHWA, for block fips 360610127002001 :  \url{http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001} \cr
#'   Notice the second one gets P2 from 10_PL not 10_SF1, because P2 in SF1 means something different !!! \cr
#'   # TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON ONE BLOCK: \cr
#'   # block fips 360610127002001 \cr
#'   # \url{http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001} \cr
#'   # TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON TWO BLOCKS: \cr
#'   # \url{http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001|1000000US360610127002000} \cr
#' @param fips vector of FIPS
#' @param censustable 'P2' by default but can be another table code
#' @param censusfile 'DEC/10_PL' by default
#' @param launch TRUE by default, whether to open page in web browser
#' @return Can open a webpage. Returns vector of URL(s) as character
#' @seealso \code{\link{url.countyhealthrankings}}
#' @examples
#'   myfips <- '360610127002001'
#'   url.censusblock(myfips, launch=FALSE)
#'   myfips <- c('360610127002001', '360610127002000')
#'   url.censusblock(myfips, launch=FALSE)
#' @export
url.censusblock <- function(fips, censustable='P2', censusfile='DEC/10_PL', launch=TRUE) {
  fips <- clean.fips.1215(fips)
  if (any(nchar(fips)!=15)) {warning('block fips must have 15 characters including leading zero if needed, and this does not work for a block group etc yet')}

  if (length(fips)==1) {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/1000000US', fips, sep='')
  } else {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/', paste( paste('1000000US',fips,sep=''), collapse='|'), sep='')
  }

  if (launch) { url.open(myurl) }
  return(myurl)
}

