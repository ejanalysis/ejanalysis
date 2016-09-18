#' @title Get information on U.S. State(s)
#' @description Query information about States, from proxistat package data in data(lookup.states, package='proxistat')
#' @details
#'   See \pkg{proxistat} package for data source (\url{http://ejanalysis.github.io/proxistat/})
#'   For 1+ or all US States plus DC, PR, Island Areas (and USA overall for use in FTP URL):\cr\cr
#'   EPA Region, FIPS, State name, abbreviation for State(s); based on any of these query methods: \cr\cr
#'   State's FIPS, State's name, OR State's abbreviation, (i.e.,  FIPS.ST, statename, or ST). \cr\cr
#'   Also see data in packages \pkg{acs} and \pkg{choroplethr} \cr\cr
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
#'   FIPS.ST, ST, statename, ftpname, REGION, is.usa.plus.pr, is.usa, is.state, is.contiguous.us, is.island.areas, and others (see below)
#' @return A data.frame (if query has 2+ elements), providing all or specified fields of information, covering matching states/dc/pr/island areas,
#'   a vector of the same type of information for just one place (if only 1 query term, i.e., one element in the query vector is provided),
#'   or NA if certain problems arise.\cr\cr
#'   If no query term, or fields not specified, then all information fields are returned: \cr
#'      get.state.info()[1:2, ]\cr\cr
#'   statename FIPS.ST ST ftpname REGION is.usa.plus.pr is.usa is.state is.contiguous.us \cr
#' 1   Alabama      01 AL Alabama      4           TRUE   TRUE     TRUE             TRUE \cr
#' 2    Alaska      02 AK  Alaska     10           TRUE   TRUE     TRUE            FALSE \cr \cr
#'
#'   is.island.areas area.sqmi area.sqkm landarea.sqmi landarea.sqkm waterarea.sqmi waterarea.sqkm \cr
#' 1           FALSE     52420    135767         50645        131171           1775           4597 \cr
#' 2           FALSE    665384   1723337        570641       1477953          94743         245383 \cr \cr
#'
#'   inland.sqmi inland.sqkm coastal.sqmi coastal.sqkm greatlakes.sqmi greatlakes.sqkm \cr
#' 1        1058        2740          517         1340               0               0 \cr
#' 2       19304       49997        26119        67647               0               0 \cr \cr
#'
#'   territorial.sqmi territorial.sqkm      lat        lon \cr
#' 1              199              516 32.73963  -86.84346 \cr
#' 2            49320           127739 63.34619 -152.83707 \cr
#' @template seealsoFIPS
#' @examples
#' # data(lookup.states, package='proxistat')
#' # x <- get.state.info(); str(x); cat('\n'); x[ 1:2, ]
#' # get.state.info(c('alaska','north carolina', 'montana', "hawaii"),
#'  fields=c('ST','statename','REGION'))
#' # get.state.info('DC'); get.state.info('U.S. Virgin Islands'); get.state.info(4)
#' # get.state.info(c('New york','alaska','North Carolina','MONTANA', 'typo'))
#' # get.state.info(c('ny','DC','AK','mt', 'PR'))
#' # get.state.info( c(36, 36, 'ny', '  ny', 'ny  ', 'California', 'DC','AK','mt', 'PR',
#'  '02', 2, 'North carolina') )
#' # get.state.info(1:80)
#' @export
get.state.info <- function(query, fields='all') {

  # ********* INFO IS NOW OBTAINED FROM proxistat package:

  data(lookup.states, package='proxistat')



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
    cat('Warning- No matches found for what should be state identifiers.\n'); return(NA)
  }

  #rownames(results) <- query  # This would not work if there were duplicates in the query vector, so create a column to show the query term
  results <- data.frame(QUERY=query, results, stringsAsFactors=FALSE)

  return(results)
}
