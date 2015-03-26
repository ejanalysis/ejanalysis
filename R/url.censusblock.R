url.censusblock <- function(fips, censustable='P2', censusfile='DEC/10_PL', launch=TRUE) {

  ##########################
  # DRAFT CODE TO see table webpage of BLOCK DATA VIA AFF WITHOUT NEEDING API KEY:
  ##########################

  if (any(nchar(fips)!=15)) {warning('block fips must have 15 characters including leading zero if needed, and this does not work for a block group etc yet')}

  if (length(fips)==1) {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/1000000US', fips, sep='')
  } else {
    myurl <- paste('http://factfinder2.census.gov/bkmk/table/1.0/en/', censusfile, '/', censustable, '/', paste( paste('1000000US',fips,sep=''), collapse='|'), sep='')
  }
  
  if (launch) { url.open(myurl) }
  return(myurl)
  
  Examples:
  myfips <- '360610127002001'
  url.censusblock(myfips)
  myfips <- c('360610127002001', '360610127002000')
  url.censusblock(myfips)

}

# notes

# For links to AFF, see http://factfinder2.census.gov/files/AFF_deep_linking_guide.pdf
# e.g. to get 1 block census 2010 pop, for block fips 360610127001000 :  http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_SF1/P1/1000000US360610127001000
# e.g. to get 1 block census 2010 RACE/ETH/NHWA, for block fips 360610127002001 :  http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001
# notice the second one gets P2 from 10_PL not 10_SF1, because P2 in SF1 means something different !!!


# TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON ONE BLOCK:
# block fips 360610127002001

# http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001


# TO GET RACE/ETHNICITY CENSUS 2010 COUNTS ON TWO BLOCKS:

# http://factfinder2.census.gov/bkmk/table/1.0/en/DEC/10_PL/P2/1000000US360610127002001|1000000US360610127002000

