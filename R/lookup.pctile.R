#' @title Find approx wtd percentiles in lookup table that is in memory
#'
#' @description This is used with lookup or us, a data.frame that is a lookup table created by
#' \code{\link{write.pctiles}}, \code{\link{write.pctiles.by.zone}}, \code{\link{write.wtd.pctiles}}, or \code{\link{write.wtd.pctiles.by.zone}}.
#' The data.frame us or lookup must have a field called "PCTILE" that contains quantiles/percentiles
#' and other column(s) with values that fall at those percentiles.
#' This function accepts lookup table (or uses one called us if that is in memory),
#' and finds the number in the PCTILE column that corresponds to where a specified value (in myvector)
#' appears in the column called varname.in.lookup.table.
#' The function just looks for where the specified value fits between values in the lookup table and returns the approximate percentile as found in the PCTILE column.
#' @param myvector Numeric vector, required. Values to look for in the lookup table.
#' @param varname.in.lookup.table Character element, required. Name of column in lookup table to look in to find interval where a given element of myvector values is.
#' @param lookup Either lookup must be specified, or a lookup table called us must already be in memory. This is the lookup table data.frame with a PCTILE column and column whose name is the value of varname.in.lookup.table.
#' @param zone Character element, optional. If specified, must appear in a column called REGION within the lookup table. For example, it could be 'NY' for New York State.
#' @return By default, returns numeric vector length of myvector.
#' @seealso \code{\link{write.pctiles}}, \code{\link{write.pctiles.by.zone}}, \code{\link{write.wtd.pctiles}}, and \code{\link{write.wtd.pctiles.by.zone}}
#' @examples
#' # places.etc$new.pctile.pm <- lookup.pctile(places.etc$countymean, "PM25")
#' @export
lookup.pctile <- function(myvector, varname.in.lookup.table, lookup, zone) {

	if (missing(lookup) & (exists('us'))) {lookup <- us}
	if (missing(lookup) & !exists('us')) {stop('must specify lookup= or have it in memory named "us"')}
  if (missing(zone) & lookup$REGION[1]!='USA') {stop('If lookup is not us, need to specify zone="NY" for example')}
	# lookup table must have PCTILE field (& this removes the row called 'mean')
	if (!('PCTILE' %in% names(lookup))) {stop('lookup must have a field called "PCTILE" that contains quantiles/percentiles')}
	lookup <- lookup[lookup$PCTILE!="std.dev", ]
	lookup <- lookup[lookup$PCTILE!="mean", ]

	if (missing(zone)) {
		return(as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , varname.in.lookup.table]) ]))
	} else {
	  if (!(zone %in% lookup$REGION)) {stop('zone not found in lookup')}
	  lookup <- lookup[lookup$REGION==zone, ]
	  return(as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , varname.in.lookup.table]) ]))
	}

}

