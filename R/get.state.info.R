#' @title Got information on state(s) **But now available as data(lookup.states) in proxistat package
#' @description Look for information from proxistat package data in data(lookup.states) -- info on states, like name, FIPS, etc.
#' @details For 1+ or all US States plus DC, PR, Island Areas (and USA overall for use in FTP URL):\cr\cr
#'   EPA Region, FIPS, State name, abbreviation for State(s); based on any of these query methods: \cr\cr
#'   State's FIPS, State's name, OR State's abbreviation, (i.e.,  FIPS.ST, statename, or ST). \cr\cr
#'   Also see data in packages \pkg[acs] \code{\link{[acs]}} and \pkg[choroplethr] \code{\link{[choroplethr]}} \cr\cr
#'   Also see \url{http://www.census.gov/geo/reference/docs/state.txt} and \url{http://www.census.gov/geo/reference/ansi.html} \cr
#'     # Note on definitions of is.usa, is.contiguous.us, etc.:  \cr
#' \url{https://www.census.gov/geo/reference/gtc/gtc_usa.html} \cr
#' \url{https://www.census.gov/geo/reference/gtc/gtc_codes.html} \cr
#' \url{https://www.census.gov/geo/reference/gtc/gtc_island.html} \cr
#' \url{http://en.wikipedia.org/wiki/Contiguous_United_States} \cr\cr
#' Also note this other possible list of abbreviations (not used) lacks US, PR, DC: \cr
#' require(datasets); state.abb \cr\cr
#' Note another possible list of States, abbrev, FIPS \cr
#' which has island areas but not US total and not leading zeroes on FIPS: \cr
#'   require(acs) \cr
#'   print(fips.state) \cr\cr
#' Note FIPS were also available here: \cr
#'  State: \url{http://www.census.gov/geo/reference/ansi_statetables.html}  \cr
#'  County: \url{http://www.census.gov/geo/www/codes/county/download.html}  \cr\cr
#' Also see \url{https://www.census.gov/geo/reference/state-area.html} for info on state area and internal point
#' @param query vector of 1+ elements, which can be \cr
#'   state FIPS code(s) (as numbers or strings with numbers), \cr
#'   state name(s) (exactly matching formats here), or \cr
#'   2-letter state abbreviation(s) (case insensitive). \cr
#' @param fields vector of 1+ character string names of the fields available here:
#'   FIPS.ST, ST, statename, ftpname, REGION, is.usa.plus.pr, is.usa, is.state, is.contiguous.us, is.island.areas
#' @return A data.frame (if query has 2+ elements), providing all or specified fields of information, covering matching states/dc/pr/island areas,
#'   a vector of the same type of information for just one place (if only 1 query term, i.e., one element in the query vector is provided),
#'   or NA if certain problems arise.\cr\cr
#'   If no query term, or fields not specified, then all information fields are returned: \cr
#'   FIPS.ST, ST, statename, ftpname, (EPA) REGION, is.usa.plus.pr, is.usa, is.state, is.contiguous.us, is.island.areas
#' @template seealsoFIPS
#' @examples
#' #  x <- get.state.info(); str(x); print(''); head(x)
#' # get.state.info(c('alaska','north carolina', 'montana', "hawaii"), fields=c('ST','statename','REGION'))
#' # get.state.info('DC'); get.state.info('U.S. Virgin Islands'); get.state.info(4)
#' # get.state.info(c('New york','alaska','North Carolina','MONTANA', 'typo'))
#' # get.state.info(c('ny','DC','AK','mt', 'PR'))
#' # get.state.info( c(36, 36, 'ny', '  ny', 'ny  ', 'California', 'DC','AK','mt', 'PR', '02', 2, 'North carolina') )
#' # get.state.info(1:80)
#' @export
get.state.info <- function(query, fields='all') {

    # ********* INFO IS NOW AVAILABLE VIA data(lookup.states) in proxistat package
  data(lookup.states, package='proxistat')

# used to read in only if not already there but now will read it in again anyway, since had been a bug in code and this ensures using newer fixed version.
#if (!exists('lookup'))  {
if (1==0)  {

  # SHOULD RECODE PERHAPS TO USE THAT OR AN  OFFICIAL LIST OF STATE NAMES AND FIPS?
  # Otherwise just keep using data below:

  # Put the data into (at least local) memory here if not already available:
  # used to use the name lookup but now using lookup.states
  lookup <- structure(list(
    FIPS.ST = c(NA, "01", "02", "04", "05", "06",
"08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19",
"20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
"31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41",
"42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54",
"55", "56", "60", "66", "69", "72", "74", "78"), ST = c("US",
"AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
"HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
"MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
"NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
"UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR",
"UM", "VI"),
    statename = c("United States", "Alabama", "Alaska",
"Arizona", "Arkansas", "California", "Colorado", "Connecticut",
"Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii",
"Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
"Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
"Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
"Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
"North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
"Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
"Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
"West Virginia", "Wisconsin", "Wyoming", "American Samoa", "Guam",
"Northern Mariana Islands", "Puerto Rico", "U.S. Minor Outlying Islands",
"U.S. Virgin Islands"),
    ftpname = c("UnitedStates", "Alabama",
"Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
"Delaware", "DistrictOfColumbia", "Florida", "Georgia", "Hawaii",
"Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
"Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
"Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
"Nevada", "NewHampshire", "NewJersey", "NewMexico", "NewYork",
"NorthCarolina", "NorthDakota", "Ohio", "Oklahoma", "Oregon",
"Pennsylvania", "RhodeIsland", "SouthCarolina", "SouthDakota",
"Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
"WestVirginia", "Wisconsin", "Wyoming", NA, NA, NA, "PuertoRico",
NA, NA),
    REGION = c(NA, 4, 10, 9, 6, 9, 8, 1, 3, 3, 4, 4, 9,
10, 5, 5, 7, 7, 4, 6, 1, 3, 1, 5, 5, 4, 7, 8, 7, 9, 1, 2, 6,
2, 4, 8, 5, 6, 10, 3, 1, 4, 8, 4, 6, 8, 1, 3, 10, 3, 5, 8, 9,
9, 9, 2, 9, 2),
    is.usa.plus.pr = c(FALSE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    is.usa = c(FALSE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE),
    is.state = c(FALSE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    is.contiguous.US = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    is.island.areas = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
    .Names = c("FIPS.ST", "ST", "statename", "ftpname", "REGION",
  "is.usa.plus.pr", "is.usa", "is.state", "is.contiguous.us", "is.island.areas"),
    class = "data.frame", row.names = c(NA, 58L))
}

######  Query & report results differently depending on nature of the query term if any:

  # If no fields specified, return all fields.
  if (any(fields=='all')) {fields <- names(lookup.states)}

  # If any bad fieldnames are specified, stop.
  if (any(!(fields %in% names(lookup.states)))) {
    cat('Available field names:\n'); cat(names(lookup.states)); cat('\n');
    stop('fields requested must all be among fields available')
  }

  # If no query term specified, return entire table of data (or just specified fields).
  if (missing(query)) { return(lookup.states[ , fields]) }

  # If query has any NA values, warn user.
  if (any(is.na(query))) {cat('Warning - some NA values in input query\n')}

####### done checking overall type of input

x <- query

# If FIPS.ST is kept at NA, it will match the NA FIPS code that is associated with USA overall in the lookup.states table, so set it to zero.
x[is.na(x)]  <- 0

# prepopulate the output variable
results <- matrix(NA, nrow=length(x), ncol=length(fields))
results <- data.frame(results)
names(results) <- fields

# remove leading and trailing blank spaces, in case those are present, so it will still match '   NY' for example
x <- gsub('^\\s+|\\s+$', '', x)

# FIND WHICH OF QUERY TERMS ARE VALID FIPS.ST AND GET STATE DATA FOR THOSE
is.valid.FIPS.ST <- grepl('^[0-9]*$', x)
is.valid.FIPS.ST[is.valid.FIPS.ST] <- as.numeric(x[is.valid.FIPS.ST]) %in% as.numeric(lookup.states$FIPS.ST)
# cbind(x, is.valid.FIPS.ST  )
# as.numeric('NY') would fail
results[is.valid.FIPS.ST, ] <- lookup.states[ match(as.numeric(x[is.valid.FIPS.ST]), as.numeric(lookup.states$FIPS.ST)), fields]

# FIND WHICH OF QUERY TERMS ARE VALID statename AND GET STATE DATA FOR THOSE
upx <- toupper(x)
is.valid.statename <- grepl('^[[:space:][:alpha:][:punct:]]*$', upx)
is.valid.statename[is.valid.statename] <- upx[is.valid.statename] %in% toupper(lookup.states$statename)
results[is.valid.statename, ] <- lookup.states[ match( upx[is.valid.statename], toupper(lookup.states$statename)), fields]

# FIND WHICH OF QUERY TERMS ARE VALID ST (2-letter abbreviation) AND GET STATE DATA FOR THOSE
upx <- toupper(x)
is.valid.ST <- grepl('^[[:space:][:alpha:]]*$', upx)
is.valid.ST[is.valid.ST] <- upx[is.valid.ST] %in% toupper(lookup.states$ST)
results[is.valid.ST, ] <- lookup.states[ match( upx[is.valid.ST], toupper(lookup.states$ST)), fields]

if (all(is.na(results[ , 1]))) {
  cat('Warning- No matches found.\n'); return(NA)
}

#rownames(results) <- query  # This would not work if there were duplicates in the query vector, so create a column to show the query term
results <- data.frame(QUERY=query, results, stringsAsFactors=FALSE)

return(results)
}

### OUTPUTS OF FUNCTION:

if (1==0) {

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
#51      55**** WI                   Wisconsin          Wisconsin      5           TRUE   TRUE     TRUE             TRUE           FALSE
#52      56 WY                     Wyoming            Wyoming      8           TRUE   TRUE     TRUE             TRUE           FALSE
#53      60 AS              American Samoa               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
#54      66 GU                        Guam               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
#55      69 MP    Northern Mariana Islands               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
#56      72 PR                 Puerto Rico         PuertoRico      2           TRUE  FALSE    FALSE            FALSE            TRUE
#57      74 UM U.S. Minor Outlying Islands               <NA>      9          FALSE  FALSE    FALSE            FALSE            TRUE
#58      78 VI         U.S. Virgin Islands               <NA>      2          FALSE  FALSE    FALSE            FALSE            TRUE


}
