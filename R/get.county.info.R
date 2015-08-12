#' @title Get info on US Counties
#'
#' @description Function that reports some or all of a table of data about US Counties (and maybe county equivalents)
#' for 1 or more counties, 1 or more States plus DC, PR, Island Areas, and USA overall.
#' Provides FIPS.COUNTY, countyname, fullname, ST based on COUNTYs FIPS (FIPS.COUNTY), or maybe countyname plus ST (STATE ABBREVIATION)
#' @details Converted this to data rather than a function, so now can just say data(counties, package='proxistat') or x<- counties via lazy loading. \cr
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
#' 1     autauga                 01001                1001    alabama        AL                   01                  1 \cr
#' 2      blount                 01009                1009    alabama        AL                   01                  1 \cr
#' 3      monroe                 01099                1099    alabama        AL                   01                  1 \cr
#' 4  washington                 01129                1129    alabama        AL                   01                  1 \cr
#' 5    marshall                 01095                1095    alabama        AL                   01                  1 \cr
#' 6      mobile                 01097                1097    alabama        AL                   01                  1 \cr
#'   county.names[county.names$county.fips.character=='02185',] \cr
#'    county.name county.fips.character county.fips.numeric state.name state.abb state.fips.character state.fips.numeric \cr
#' 88 north slope                 02185                2185     alaska        AK                   02                  2 \cr \cr
#'   > head(get.county.info()) \cr
#'   ST     countyname FIPS.COUNTY statename                fullname \cr
#' 1 AL Autauga County       01001   Alabama Autauga County, Alabama \cr
#' 2 AL Baldwin County       01003   Alabama Baldwin County, Alabama \cr
#' 3 AL Barbour County       01005   Alabama Barbour County, Alabama \cr
#' 4 AL    Bibb County       01007   Alabama    Bibb County, Alabama \cr
#' 5 AL  Blount County       01009   Alabama  Blount County, Alabama \cr
#' 6 AL Bullock County       01011   Alabama Bullock County, Alabama \cr
#'   get.county.info()[get.county.info()$FIPS.COUNTY=='02185',] \cr
#'    ST          countyname FIPS.COUNTY statename                    fullname \cr
#' 85 AK North Slope Borough       02185    Alaska North Slope Borough, Alaska \cr \cr
#'  State,State ANSI,County ANSI,County Name,ANSI Cl \cr
#'  AL,01,001,Autauga County,H1 \cr
#'  AL,01,003,Baldwin County,H1 \cr \cr
#' Also see:
#' \itemize{
#' \item \url{http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt}
#' \item \url{http://www.census.gov/geo/reference/ansi.html}
#' \item \url{http://www.census.gov/geo/reference/codes/files/national_county.txt}
#' \item \url{http://www.census.gov/geo/reference/docs/state.txt} for just state info
#' \item State: \url{http://www.census.gov/geo/reference/ansi_statetables.html}
#' \item County: \url{http://www.census.gov/geo/www/codes/county/download.html}
#' \item Note on definitions of is.usa, is.contiguous.us, etc.:
#' \item \url{https://www.census.gov/geo/reference/gtc/gtc_usa.html}
#' \item \url{https://www.census.gov/geo/reference/gtc/gtc_codes.html}
#' \item \url{https://www.census.gov/geo/reference/gtc/gtc_island.html}
#' }
#' Note this other possible list of abbreviations (not used) lacks US, PR, DC:  \cr
#' require(datasets); state.abb \cr
#' Note another possible list of States, abbrev, FIPS
#' which has island areas but not US total and not leading zeroes on FIPS: \cr
#' require(acs) \cr
#' print(fips.state) \cr
#' @param query Vector of search terms.
#'   Can be county's 5-digit FIPS code(s) (as numbers or strings with numbers),
#'   maybe also could be county name(s) (exactly matching formats here), along with 2-letter state abbrev (case-insensitive)?
#' @param fields Character string optional defaults to 'all' but can specify 'countyname' 'ST' and/or 'FIPS.COUNTY'
#' @return Returns a data.frame or vector of results depending on fields selected.
#'   Returns a data.frame (if query has 2+ elements), providing all or specified fields of information, covering matching counties,
#'   or a vector of the same type of information for just one place (if only 1 query term, i.e., one element in the query vector is provided),
#'   or NA if certain problems arise.
#' If no query term, or fields not specified, then all information fields are returned:
#' ST     countyname FIPS.COUNTY statename                fullname
#' @template seealsoFIPS
#' @examples
#'   x <- get.county.info(); str(x); print(''); head(x)
#'   get.county.info(c('05001','01005'), fields=c('countyname', 'ST'))
#' @export
get.county.info <- function(query, fields='all', download=FALSE) {

  #if (!exists('lookup.county'))  {

    # could Put the data into (at least local) memory here if not already available, but no?
    # At least see if it is on disk in this folder already.

    if (download) {

      download.file( 'http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt' , 'countyinfo.txt')
      #' As of 3/2015, list is here:      http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt
      #' Prior to that it had been here: 'http://www.census.gov/geo/reference/codes/files/national_county.txt'
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
      data(countiesall, package='proxistat')
      lookup.county <- countiesall  # lazy load from data() in proxistat package
   }
  #}

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

  # remove leading and trailing blank spaces, in case those are present, so it will still match '   NY' for example
  x <- gsub('^\\s+|\\s+$', '', x)

  # FIND WHICH OF QUERY TERMS ARE VALID FIPS.COUNTY AND GET DATA FOR THOSE
  is.valid.FIPS.COUNTY <- grepl('^[0-9]*$', x)
  is.valid.FIPS.COUNTY[is.valid.FIPS.COUNTY] <- as.numeric(x[is.valid.FIPS.COUNTY]) %in% as.numeric(lookup.county$FIPS.COUNTY)
  results[is.valid.FIPS.COUNTY, ] <- lookup.county[ match(as.numeric(x[is.valid.FIPS.COUNTY]), as.numeric(lookup.county$FIPS.COUNTY)), fields]

  # could add code here to accept countyname/ST or countyname/statename combo as input


  # FIND WHICH OF QUERY TERMS ARE VALID ST (2-letter abbreviation) AND GET STATE DATA FOR THOSE
  #  upx <- toupper(x)
  #  is.valid.ST <- grepl('^[[:space:][:alpha:]]*$', upx)
  #  is.valid.ST[is.valid.ST] <- upx[is.valid.ST] %in% toupper(lookup.states$ST)
  #  results[is.valid.ST, ] <- lookup.states[ match( upx[is.valid.ST], toupper(lookup.states$ST)), fields]

  upx <- toupper(x)
  is.valid.countyname <- grepl('^[[:space:][:alpha:][:punct:]]*$', upx)
  is.valid.countyname[is.valid.countyname] <- upx[is.valid.countyname] %in% toupper(lookup.county$county)
  results[is.valid.countyname, ] <- lookup.county[ match( upx[is.valid.countyname], toupper(lookup.county$countyname)), fields]

  # FIND WHICH OF QUERY TERMS ARE VALID statename AND GET DATA FOR THOSE - but note a state would return multiple rows / counties, not just one!
  upx <- toupper(x)
  is.valid.statename <- grepl('^[[:space:][:alpha:][:punct:]]*$', upx)
  is.valid.statename[is.valid.statename] <- upx[is.valid.statename] %in% toupper(lookup.county$statename)
  # the match() function finds only the first match, not all of the matches! must use grep or grepl here
  all.counties.in.states <- NULL
  for (i in 1:sum(upx[is.valid.statename])) {
    thisone <- upx[is.valid.statename][i]
    all.counties.in.states <- rbind(all.counties.in.states, cbind(QUERY=query[is.valid.statename][i], lookup.county[ grepl(thisone, toupper(lookup.county$statename)) , fields]))
  }
  #all.counties.in.states <- sapply(upx[is.valid.statename], function(thisone) {lookup.county[ grepl(thisone, toupper(lookup.county$statename)) , fields]})
  #
  # debugging:
  #print('hi') ; print(head(is.valid.statename,100)); print(head(upx,100)); print(str(upx[is.valid.statename])); print(all.counties.in.states); print(head(lookup.county))
  #

  if (all(is.na(results[ , 1]))) {
    cat('Warning- No matches found.\n'); return(NA)
  }

  results <- data.frame(QUERY=query, results, stringsAsFactors=FALSE)

  results.extralines  <- rbind(results, all.counties.in.states)


  return(results)

  ### OUTPUTS OF FUNCTION:

  if (1==0) {

    head(get.county.info())

    #   ST     countyname FIPS.COUNTY statename                fullname
    #   1 AL Autauga County       01001   Alabama Autauga County, Alabama
    #   2 AL Baldwin County       01003   Alabama Baldwin County, Alabama

    get.state.info()

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
