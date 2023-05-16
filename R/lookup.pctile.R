#' @title Find approx wtd percentiles in lookup table that is in memory
#'
#' @description This is used with lookup or us, a data.frame that is a lookup table created by
#' [write.pctiles()], [write.pctiles.by.zone()], [write.wtd.pctiles()], or [write.wtd.pctiles.by.zone()].
#' The data.frame us or lookup must have a field called "PCTILE" that contains quantiles/percentiles
#' and other column(s) with values that fall at those percentiles.
#' This function accepts lookup table (or uses one called us if that is in memory),
#' and finds the number in the PCTILE column that corresponds to where a specified value (in myvector)
#' appears in the column called varname.in.lookup.table.
#' The function just looks for where the specified value fits between values in the lookup table and returns the approximate percentile as found in the PCTILE column.
#' If the value is between the cutpoints listed as percentiles 89 and 90, it returns 89, for example.
#' If the value is exactly equal to the cutpoint listed as percentile 90, it returns percentile 90.
#' If the value is less than the cutpoint listed as percentile 0, which should be the minimum value in the dataset,
#' it still returns 0 as the percentile, but with a warning that the value checked was less than the minimum in the dataset.
#' @param myvector Numeric vector, required. Values to look for in the lookup table.
#' @param varname.in.lookup.table Character element, required. Name of column in lookup table to look in to find interval where a given element of myvector values is.
#' @param lookup Either lookup must be specified, or a lookup table called us must already be in memory. This is the lookup table data.frame with a PCTILE column and column whose name is the value of varname.in.lookup.table.
#' @param zone Character element (or vector as long as myvector), optional. If specified, must appear in a column called REGION within the lookup table. For example, it could be 'NY' for New York State.
#' @return By default, returns numeric vector length of myvector.
#' @seealso [write.pctiles()], [write.pctiles.by.zone()], [write.wtd.pctiles()], and [write.wtd.pctiles.by.zone()]
#' @examples
#'   lookup.pctile(myvector = c(-99, 0, 500, 801, 949.9, 1000, 99999),
#'     lookup = data.frame(PCTILE = 0:100,
#'     myvar = 10*(0:100),
#'     REGION = 'USA', stringsAsFactors = FALSE), varname.in.lookup.table = 'myvar')
#'  \dontrun{
#'    What is environmental score at given percentile?
#'  ejanalysis::lookup.pctile(40,'cancer',lookupUSA)
#'  #   84
#'  ejanalysis::lookup.pctile(40,'cancer',lookupStates,'WV')
#'  #   93
#'  #    What is percentile of given environmental score?
#'  ejscreen::lookupUSA[lookupUSA$PCTILE=='84' ,'cancer']
#'  #   39.83055
#'  ejscreen::lookupStates[lookupStates$PCTILE=='84' & lookupStates$REGION =='WV','cancer']
#'  #   33.36371
#'  # also see ejanalysis::assign.pctiles
#'  }
#'   \dontrun{
#'      library(ejscreen)
#'        evarname <- 'traffic.score'
#'        evalues <- bg21[ , evarname]
#'        emax <- max(evalues,na.rm = TRUE)
#'        plot(lookupUSA[,evarname], lookupUSA$PCTILE, xlab = evarname,
#'        main='contents of lookup table', type='b')
#'
#'        plot(x = (1:1000) * (emax / 1000),
#'         y = lookup.pctile(myvector = (1:1000) * emax / 1000, varname.in.lookup.table = evarname,
#'         lookup = lookupUSA),
#'         ylim = c(0,100),   type='b',
#'         xlab=evarname, ylab='percentile per lookup table' ,
#'         main='Percentiles for various envt scores, as looked up in lookup table')
#'       abline(v = lookupUSA[ lookupUSA$PCTILE == 'mean', evarname])
#'
#'   }
#' @export
lookup.pctile <- function(myvector, varname.in.lookup.table, lookup, zone) {

	if (missing(lookup) & (exists('us'))) {lookup <- us}
	if (missing(lookup) & !exists('us')) {stop('must specify lookup= or have it in memory named "us"')}
  if (missing(zone) & lookup$REGION[1] != 'USA') {stop('If lookup is not us, need to specify zone="NY" for example')}
	# lookup table must have PCTILE field (& this removes the row called 'mean')
	if (!('PCTILE' %in% names(lookup))) {stop(
	  'lookup must have a field called "PCTILE" that contains quantiles/percentiles')}
  lookup <- lookup[lookup$PCTILE != "std.dev", ] #   slow - should drop before using the dataset
	lookup <- lookup[lookup$PCTILE != "mean", ]

	if (!(varname.in.lookup.table %in% colnames(lookup))) {
	  warning(paste0(varname.in.lookup.table, " must be a column in lookup table"))
	  return(rep(NA, length(myvector)))
	} else {
	  if (missing(zone)) {

	    whichinterval <- findInterval(myvector, lookup[ , varname.in.lookup.table])

	  } else {
	    if (any(!(zone %in% lookup$REGION))) {stop('zone(s) not found in lookup')}

	    # also see similar code in ejanalysis::lookup.pctiles() !

	    whichinterval <- vector(length = NROW(myvector))

	    for (z in unique(zone)) {
	      #  for each zone
	      myvector_selection <- myvector[zone == z] # sort(myvector)
	      myvector_lookup <-   lookup[lookup$REGION == z, varname.in.lookup.table]

	      # should be OK if some or all those values in myvector are NA?
	      #  if some or all in lookup are NA, though it crashes unless that case is handled
	      if (any(is.na(myvector_lookup))) {
	        whichinterval[zone == z] <- rep(NA, length(myvector_selection))
	        warning("No percentile info available for ", varname.in.lookup.table, " in ", z)

	 # *** THIS WILL STILL FAIL - JUST SETTING whichinterval TO NA may not work below
	        # where it tries to use it to subset...  lookup$PCTILE[whichinterval]

	      } else {
	        whichinterval[zone == z] <- findInterval(myvector_selection, myvector_lookup)
	      }
	    }
	  }
	}


	# *** BELOW SEEMS WRONG IF WE ARE DOING IT ZONE BY ZONE ABOVE, BUT NOT BELOW


	# would be an error if zeroeth row were selected here,
	# so just say it is at the lowest percentile listed (which is 0)
	# even if it is below the minimum value that supposedly defines the lower edge
	belowmin <- (whichinterval == 0)
	if (any(belowmin, na.rm = TRUE)) {
	  whichinterval[!is.na(belowmin) & belowmin]  <- 1
	  warning('One or more values were below the minimum, or zeroeth percentile, but are reported by this function as being at the 0 percentile.')
	}
	whichinterval[is.na(belowmin)] <- NA
	# returns NA if belowmin is NA



	return(as.numeric(lookup$PCTILE[whichinterval]))
}








