#' @title Convert between FIPS / ANSI codes and Names of U.S. Geographies
#' @description
#'   Tries to interpret vector of one or more FIPS codes and/or names of geographies,
#'   and convert between them.
#' @param x Required vector of one or more numeric or character FIPS and/or names of geographic locations.
#'   Allowed types are State, County (or equivalent), tract, block group, and block.
#'   Names for tracts, blockgroups, and blocks are not provided or interpreted.
#'   FIPS codes here are all the relevant digits starting with the 2-character state FIPS,
#'   so county fips must be 4-5 digits or characters for example (leading zeroes are inferred where
#'   possible and included in outputs). See \code{\link{clean.fips}} for details.
#' @param to Optional. When the to parameter is not specified, then x that appear to be fips are converted to name, and x that appear to be names are converted to fips.
#'   Can specify vector of types of geographies (state, etc.) to convert fips to.
#'   This is useful to find County or State names for a list of tract fips, for example.
#'   The to parameter can be fips (which converts names to any appropriate type of fips),
#'   name (which converts to appropriate type of name based on fips length),
#'   specific type of name including state, county, tract, bg (or blockgroup), or block, or
#'   specific type of fips, including fips.st, fips.county, fips.tract, fips.bg, fips.block.
#'   Names for tracts, blockgroups, and blocks are not provided or interpreted.
#'   Variations also work such as plural and case-insensitive.
#' @return Returns vector of same length as x, containing fips as character elements with any leading zeroes, and/or names of geographies.
#' @seealso \code{\link[ejanalysis]{get.fips.st}} and related functions noted there, \code{\link[ejanalysis]{clean.fips}}, \code{\link[ejanalysis]{get.state.info}}
#' @examples
#'    geofips(c('NY', 'Alabama', 1, 14, 'Montgomery County, Maryland',
#'     '01121', 1121, '060690006002', 60690006002, '011210118001025')
#'    geofips(c('01121', 'Montgomery County, Maryland'), c('state', 'fips.st')
#' @export
geofips <- function(x, to) {
  stop('not done yet')
  if (1==0) {
    xx <- x

    tryfips <- clean.fips(x)
    isfips <- !is.na(tryfips)
    xx[isfips] <- tryfips[isfips]

    trygeo <- clean.geo(x)
    isgeo <- !is.na(trygeo)
    xx[isgeo] <- trygeo[isgeo]

    xx[isfips + isgeo == 0] <- NA



    if (missing(to)) {
      xx[isgeo]  <- geotofips(xx[isgeo], type=geotype(xx[isgeo]))
      xx[isfips] <- fipstogeo(xx[isgeo])
    } else {
      # to was specified
      to <- 0


    }
  }

}
