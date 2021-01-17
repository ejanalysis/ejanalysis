#' @title Get info on US Counties
#'
#' @description Function that reports some or all of a table of data
#'   about queried (or all) US Counties (and county equivalents)
#'   for 1 or more counties. Query terms can be 5-digit FIPS,
#'   or 'countyname, statename', or just statename or just 2-letter state abbrev.
#'   'Montgomery, MD' will not work. 'Montgomery County, Maryland' will work.
#'
#'   Requested fields can include any of these: "ST", "countyname", "FIPS.COUNTY", "statename", "fullname"
#' @details  Converted basic data to data, so now can also say data(counties, package='proxistat') or x <- countiesall via lazy loading. \cr
#'   Also, as of 3/2015, a list is here: \url{http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt} \cr
#'   help(county.names, package='choroplethr') \cr
#'   data(county.names) \cr
#'   compare that function to this one: \cr
#'   > length(county.names[,1]) \cr
#'   [1] 3142 \cr
#'   > length(get.county.info()[,1]) \cr
#'   [1] 3235 \cr
#'   > head(county.names) \cr
#'   county.name county.fips.character county.fips.numeric state.name state.abb state.fips.character state.fips.numeric \cr
#'   1     autauga                 01001                1001    alabama        AL                   01                  1 \cr
#'   2      blount                 01009                1009    alabama        AL                   01                  1 \cr
#'   3      monroe                 01099                1099    alabama        AL                   01                  1 \cr
#'   4  washington                 01129                1129    alabama        AL                   01                  1 \cr
#'   5    marshall                 01095                1095    alabama        AL                   01                  1 \cr
#'   6      mobile                 01097                1097    alabama        AL                   01                  1 \cr
#'   county.names[county.names$county.fips.character=='02185',] \cr
#'      county.name county.fips.character county.fips.numeric state.name state.abb state.fips.character state.fips.numeric \cr
#'   88 north slope                 02185                2185     alaska        AK                   02                  2 \cr \cr
#'   > head(get.county.info()) \cr
#'     ST     countyname FIPS.COUNTY statename                fullname \cr
#'   1 AL Autauga County       01001   Alabama Autauga County, Alabama \cr
#'   2 AL Baldwin County       01003   Alabama Baldwin County, Alabama \cr
#'   3 AL Barbour County       01005   Alabama Barbour County, Alabama \cr
#'   4 AL    Bibb County       01007   Alabama    Bibb County, Alabama \cr
#'   5 AL  Blount County       01009   Alabama  Blount County, Alabama \cr
#'   6 AL Bullock County       01011   Alabama Bullock County, Alabama \cr
#'     get.county.info()[get.county.info()$FIPS.COUNTY=='02185',] \cr
#'      ST          countyname FIPS.COUNTY statename                    fullname \cr
#'   85 AK North Slope Borough       02185    Alaska North Slope Borough, Alaska \cr \cr
#'    State,State ANSI,County ANSI,County Name,ANSI Cl \cr
#'    AL,01,001,Autauga County,H1 \cr
#'    AL,01,003,Baldwin County,H1 \cr \cr
#'   Also see:
#'   \itemize{
#'   \item \url{http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt}
#'   \item \url{http://www.census.gov/geo/reference/ansi.html}
#'   \item \url{http://www.census.gov/geo/reference/codes/files/national_county.txt}
#'   \item \url{http://www.census.gov/geo/reference/docs/state.txt} for just state info
#'   \item State: \url{http://www.census.gov/geo/reference/ansi_statetables.html}
#'   \item County: \url{http://www.census.gov/geo/www/codes/county/download.html}
#'   \item Note on definitions of is.usa, is.contiguous.us, etc.:
#'   \item \url{https://www.census.gov/geo/reference/gtc/gtc_usa.html}
#'   \item \url{https://www.census.gov/geo/reference/gtc/gtc_codes.html}
#'   \item \url{https://www.census.gov/geo/reference/gtc/gtc_island.html}
#'   }
#'   Note this other possible list of abbreviations (not used) lacks US, PR, DC:  \cr
#'   require(datasets); state.abb \cr
#'   Note another possible list of States, abbrev, FIPS
#'   which has island areas but not US total and not leading zeroes on FIPS: \cr
#'   require(acs) \cr
#'   print(fips.state) \cr
#' @param query Vector of search terms.
#'   Can be county's 5-digit FIPS code(s) (as numbers or strings with numbers),
#'   and also could be 'countyname, statename' (fullname, exactly matching formats in countiesall$fullname, but case insensitive).
#' @param fields Character string optional defaults to 'all' but can specify 'countyname' 'ST' and/or 'FIPS.COUNTY'
#' @param download default is FALSE. If TRUE, download file from census website to local working directory, save as countyinfo.txt
#' @return Returns a data.frame or vector of results depending on fields selected.
#' Returns a data.frame (if query has 2+ elements), 'QUERY' as first column, and then all or specified fields of information, covering matching counties,
#' or NA if certain problems arise.
#' If no query term, or fields not specified, then all information fields are returned:
#' QUERY, ST, countyname, FIPS.COUNTY, statename, fullname
#'
#' @examples
#'  testdata <- c('01001', 1001, '1001', "Montgomery County, Maryland", "Montgomery County, MD", 'montgomery county, maryland', "Montgomery County MD", "MontgomeryCountyMD", "Montgomery County", "NY")
#'  testonlystates <- c('NY', 'NJ')
#'  get.county.info(testdata)
#'  get.county.info(testonlystates)
#'  get.county.info(c('New Jersey'))
#'
#' @export
get.county.info <- function(query, fields = 'all', download = FALSE) {

  if (download) {
    download.file( 'http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt' , 'countyinfo.txt')
    # As of 3/2015, list is here:      http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
    # Prior to that it had been here: 'http://www.census.gov/geo/reference/codes/files/national_county.txt'
    x=read.csv('countyinfo.txt', header=FALSE, as.is=TRUE)
    ##### State,State ANSI,County ANSI,County Name,ANSI Cl
    names(x) <- c('ST', 'FIPS.ST', 'FIPS.COUNTY.3', 'countyname', 'junk')
    x$junk <- NULL
    x$FIPS.COUNTY.3 <- analyze.stuff::lead.zeroes(x$FIPS.COUNTY.3, 3)
    x$FIPS.ST <- analyze.stuff::lead.zeroes(x$FIPS.ST, 2)
    x$FIPS.COUNTY <- analyze.stuff::lead.zeroes(paste(x$FIPS.ST, x$FIPS.COUNTY.3, sep=''), 5)
    x$FIPS.COUNTY.3 <- NULL
    x$FIPS.ST <- NULL

    # was doing this, which uses ejanalysis function get.state.info which in turn uses data(lookup.states, package='proxistat')
    # x$statename <- ejanalysis::get.state.info(x$ST)[ , 'statename']
    # now instead just do this, using proxistat but not ejanalysis pkg:
    data(lookup.states, package='proxistat')
    x$statename <- lookup.states[ match(x[ , 'ST'], lookup.states[ , 'ST']), 'statename']

    x$fullname <- apply( x[ , c('countyname', 'statename')] , 1, function(myrow) paste(myrow[1], myrow[2], sep=', '))
    lookup.county <- x
  } else {
    # but lazy load from data() in proxistat package is  a better way
    data(countiesall, package='proxistat')
    lookup.county <- countiesall
  }

  ######  Query & report results differently depending on nature of the query term if any:

  # If no fields specified, return all fields.
  if (any(fields=='all')) {fields <- names(lookup.county)}

  # If any bad fieldnames are specified, stop.
  if (any(!(fields %in% names(lookup.county)))) {
    cat('Available field names:\n'); cat(names(lookup.county)); cat('\n');
    stop('fields requested must all be among fields available')
  }

  # If no query term specified, return entire table of data (or just specified fields).
  if (missing(query)) { return(lookup.county[ , fields]) }

  # If query has any NA values, warn user.
  if (any(is.na(query))) {cat('Warning - some NA values in input query\n')}

  ####### done checking overall type of input

  x <- query

  # If FIPS.ST is kept at NA, it will match the NA FIPS code that is associated with USA overall in the lookup table, so set it to zero.
  # x[is.na(x)]  <- 0

  # prepopulate the output variable
  results <- matrix(NA, nrow=length(x), ncol=length(fields))
  results <- data.frame(results)
  names(results) <- fields

  # FIND WHICH OF QUERY TERMS ARE VALID FIPS.COUNTY AND GET DATA FOR THOSE
  is.valid.FIPS.COUNTY <- grepl('^[0-9]*$', x) # verify it is numeric as character
  is.valid.FIPS.COUNTY[is.valid.FIPS.COUNTY] <- as.numeric(x[is.valid.FIPS.COUNTY]) %in% as.numeric(lookup.county$FIPS.COUNTY)
  results[is.valid.FIPS.COUNTY, ] <- lookup.county[ match(as.numeric(x[is.valid.FIPS.COUNTY]), as.numeric(lookup.county$FIPS.COUNTY)), fields]

  # now that FIPS are handled, look for text queries
  # testdata <- c('01001', 1001, '1001', "Montgomery County, Maryland", "Montgomery County, MD", 'montgomery county, maryland', "Montgomery County MD", "MontgomeryCountyMD", "Montgomery County" )
  # get.county.info(testdata)

  # remove leading and trailing blank spaces, in case those are present, so it will still match '   NY' for example
  x <- gsub('^\\s+|\\s+$', '', x)  # or could use trimws() ?

  # might as well allow matching even if the comma and or any spaces are is missing
  nocomma <- function(z) gsub(pattern = ',', '', z)
  nospace <- function(z) gsub(pattern = ' ', '', z)
  x <- nocomma(nospace(x))

  # make it case-insensitive
  upx <- toupper(x)

  # FIND WHICH OF QUERY TERMS ARE VALID countyname, statename pair. Note they are not unique without statename as well!
  # Also see code in other functions that tries to parse that. (urls.countyhealthrankings()?)
  #  only "St. Croix County, Wisconsin" etc will work
  # Montgomery, MD will not work right here. Montgomery County, Maryland will work.
  is.valid.countyname <- grepl('^[[:space:][:alpha:][:punct:]]*$', upx) # verify just spaces alpha or punctuation like .
  fullnamelookup <- nocomma(nospace(toupper(lookup.county$fullname)))
  is.valid.countyname[is.valid.countyname] <- upx[is.valid.countyname] %in% fullnamelookup
  results[is.valid.countyname, ] <- lookup.county[ match( upx[is.valid.countyname], fullnamelookup), fields]

  # FIND WHICH OF QUERY TERMS ARE VALID countyname, ST (2-letter abbreviation) pair, AND GET DATA FOR THOSE
  countySTlookup <- paste(lookup.county$countyname, ', ', lookup.county$ST, sep = '')
  countySTlookup <- nocomma(nospace(toupper(countySTlookup)))
  is.valid.countyST <- upx %in% countySTlookup
  results[is.valid.countyST, ] <- lookup.county[ match(upx[is.valid.countyST], countySTlookup), fields]

  ############################################################### #

  ####### COULD MERGE ST AND statename code so it will accept a mix like c('NY', 'Alabama')


  # work in progress here...



  statenamelookup <- nocomma(nospace(toupper(lookup.county$statename)))
  is.valid.statename <- upx %in% statenamelookup
  is.valid.ST <- upx %in% (ejanalysis::get.state.info()$ST)
  if (any(is.valid.ST | is.valid.statename)) {


    #  ST
    #
    # # FIND WHICH OF QUERY TERMS ARE VALID ST (2-letter abbreviation) - BUT NO SINGLE COUNTY SPECIFIED - AND GET STATE DATA FOR THOSE
    # BUT THIS ONLY MAKES SENSE AND IS EASY TO HANDLE and is allowed here IF THE QUERY IS FOR ONE OR MORE ENTIRE STATES, NOT A MIX OF FULL STATES AND INDIVIDUAL COUNTIES
    # *** note a state will return multiple rows / counties, not just one!
    #
    if (all(is.valid.ST | is.valid.statename)) {

      #  convert all to standardized ST

      upx <- get.state.info(upx)$ST
      blah <- data.frame(QUERY = query, upx = upx, stringsAsFactors = FALSE)
      results <- merge(lookup.county, blah, by.x = 'ST', by.y = 'upx' )
      # BUT THE SORT ORDER WILL DIFFER FROM ORIGINAL QUERY !!! *************** BUT WANT SAME IN CASE WANT STATE MAP WITH ON VALUE PER WHOLE STATE
      results <- results[ , c('QUERY', fields)]
      return(results)

      # obsolete... soon...
      #
      # if (all(is.valid.ST)) {
      #   results <- data.frame(lookup.county[lookup.county$ST %in% upx, fields], stringsAsFactors = FALSE)
      #   # presumes ST is in fields which it actually may not be: could fix that when I get a chance
      #   foundstatename <- lookup.county[lookup.county$ST %in% upx, 'statename']
      #   results <- data.frame(QUERY = foundstatename, results, stringsAsFactors = FALSE)
      #   return(results) # stops here since format is different than if each query element returns one county
    } else {
      # mix of states and counties - not good
      warning('All or none of queried places must be a full state abbreviation like NY or statename like New York - if others are individual counties, the full state queries will be ignored')

    }

  }
  #
  #   #  statename
  #   #
  #   # FIND WHICH OF QUERY TERMS ARE VALID statename - BUT NO SINGLE COUNTY SPECIFIED - AND GET DATA FOR THOSE
  #   # BUT THIS ONLY MAKES SENSE AND IS EASY TO HANDLE and is allowed here IF THE QUERY IS FOR ONE OR MORE ENTIRE STATES, NOT A MIX OF FULL STATES AND INDIVIDUAL COUNTIES
  #   # *** note a state will return multiple rows / counties, not just one!
  #   statenamelookup <- nocomma(nospace(toupper(lookup.county$statename)))
  #   is.valid.statename <- upx %in% statenamelookup
  #   if (any(is.valid.statename)) {
  #
  #     if (all(is.valid.statename)) {
  #       results <- data.frame(lookup.county[statenamelookup %in% upx, fields], stringsAsFactors = FALSE)
  #       # presumes ST is in fields which it actually may not be: could fix that when I get a chance
  #       foundST <- lookup.county[statenamelookup %in% upx, 'ST']
  #       results <- data.frame(QUERY = foundST, results, stringsAsFactors = FALSE)
  #       return(results) # stops here since format is different than if each query element returns one county
  #     } else {
  #       warning('All or none of queried places must be a full state abbreviation like NY or statename like New York - if others are individual counties, the full state queries will be ignored')
  #     }
  #   }
  ############################################################### #

  if (all(is.na(results[ , 1]))) {
    cat('Warning- No matches found for what should be county or state identifiers.\n'); return(NA)
  }

  results <- data.frame(QUERY = query, results, stringsAsFactors = FALSE)
  return(results)

  ### OUTPUTS OF FUNCTION:

  if (1 == 0) {

    #    head(get.county.info())

    #   ST     countyname FIPS.COUNTY statename                fullname
    #   1 AL Autauga County       01001   Alabama Autauga County, Alabama
    #   2 AL Baldwin County       01003   Alabama Baldwin County, Alabama

    #     get.state.info()

    #   FIPS.ST ST                   statename            ftpname REGION is.usa.plus.pr is.usa is.state is.contiguous.us is.island.areas
    #1     <NA> US               United States       UnitedStates     NA          FALSE  FALSE    FALSE            FALSE           FALSE
    #2       01 AL                     Alabama            Alabama      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #3       02 AK                      Alaska             Alaska     10           TRUE   TRUE     TRUE            FALSE           FALSE
    #4       04 AZ                     Arizona            Arizona      9           TRUE   TRUE     TRUE             TRUE           FALSE
    #5       05 AR                    Arkansas           Arkansas      6           TRUE   TRUE     TRUE             TRUE           FALSE
    #6       06 CA                  California         California      9           TRUE   TRUE     TRUE             TRUE           FALSE
    #7       08 CO                    Colorado           Colorado      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #8       09 CT                 Connecticut        Connecticut      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #9       10 DE                    Delaware           Delaware      3           TRUE   TRUE     TRUE             TRUE           FALSE
    #10      11 DC        District of Columbia DistrictOfColumbia      3           TRUE   TRUE    FALSE             TRUE           FALSE
    #11      12 FL                     Florida            Florida      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #12      13 GA                     Georgia            Georgia      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #13      15 HI                      Hawaii             Hawaii      9           TRUE   TRUE     TRUE            FALSE           FALSE
    #14      16 ID                       Idaho              Idaho     10           TRUE   TRUE     TRUE             TRUE           FALSE
    #15      17 IL                    Illinois           Illinois      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #16      18 IN                     Indiana            Indiana      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #17      19 IA                        Iowa               Iowa      7           TRUE   TRUE     TRUE             TRUE           FALSE
    #18      20 KS                      Kansas             Kansas      7           TRUE   TRUE     TRUE             TRUE           FALSE
    #19      21 KY                    Kentucky           Kentucky      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #20      22 LA                   Louisiana          Louisiana      6           TRUE   TRUE     TRUE             TRUE           FALSE
    #21      23 ME                       Maine              Maine      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #22      24 MD                    Maryland           Maryland      3           TRUE   TRUE     TRUE             TRUE           FALSE
    #23      25 MA               Massachusetts      Massachusetts      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #24      26 MI                    Michigan           Michigan      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #25      27 MN                   Minnesota          Minnesota      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #26      28 MS                 Mississippi        Mississippi      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #27      29 MO                    Missouri           Missouri      7           TRUE   TRUE     TRUE             TRUE           FALSE
    #28      30 MT                     Montana            Montana      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #29      31 NE                    Nebraska           Nebraska      7           TRUE   TRUE     TRUE             TRUE           FALSE
    #30      32 NV                      Nevada             Nevada      9           TRUE   TRUE     TRUE             TRUE           FALSE
    #31      33 NH               New Hampshire       NewHampshire      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #32      34 NJ                  New Jersey          NewJersey      2           TRUE   TRUE     TRUE             TRUE           FALSE
    #33      35 NM                  New Mexico          NewMexico      6           TRUE   TRUE     TRUE             TRUE           FALSE
    #34      36 NY                    New York            NewYork      2           TRUE   TRUE     TRUE             TRUE           FALSE
    #35      37 NC              North Carolina      NorthCarolina      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #36      38 ND                North Dakota        NorthDakota      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #37      39 OH                        Ohio               Ohio      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #38      40 OK                    Oklahoma           Oklahoma      6           TRUE   TRUE     TRUE             TRUE           FALSE
    #39      41 OR                      Oregon             Oregon     10           TRUE   TRUE     TRUE             TRUE           FALSE
    #40      42 PA                Pennsylvania       Pennsylvania      3           TRUE   TRUE     TRUE             TRUE           FALSE
    #41      44 RI                Rhode Island        RhodeIsland      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #42      45 SC              South Carolina      SouthCarolina      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #43      46 SD                South Dakota        SouthDakota      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #44      47 TN                   Tennessee          Tennessee      4           TRUE   TRUE     TRUE             TRUE           FALSE
    #45      48 TX                       Texas              Texas      6           TRUE   TRUE     TRUE             TRUE           FALSE
    #46      49 UT                        Utah               Utah      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #47      50 VT                     Vermont            Vermont      1           TRUE   TRUE     TRUE             TRUE           FALSE
    #48      51 VA                    Virginia           Virginia      3           TRUE   TRUE     TRUE             TRUE           FALSE
    #49      53 WA                  Washington         Washington     10           TRUE   TRUE     TRUE             TRUE           FALSE
    #50      54 WV               West Virginia       WestVirginia      3           TRUE   TRUE     TRUE             TRUE           FALSE
    #51      56 WI                   Wisconsin          Wisconsin      5           TRUE   TRUE     TRUE             TRUE           FALSE
    #52      56 WY                     Wyoming            Wyoming      8           TRUE   TRUE     TRUE             TRUE           FALSE
    #53      60 AS              American Samoa               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
    #54      66 GU                        Guam               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
    #55      69 MP    Northern Mariana Islands               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
    #56      72 PR                 Puerto Rico         PuertoRico      2           TRUE  FALSE    FALSE            FALSE            TRUE
    #57      74 UM U.S. Minor Outlying Islands               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
    #58      78 VI         U.S. Virgin Islands               <NA>      2          FALSE  FALSE    FALSE            FALSE            TRUE

  }
}
