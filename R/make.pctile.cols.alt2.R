#' @title Alternative way to make columns of (weighted) percentiles from columns of values
#'
#' @description Takes a data.frame of values and returns a data.frame of percentiles,
#' showing the percentile of a value within all values in its column.
#' @param raw.data.frame Data.frame of values
#' @param weights Optional Numeric vector of weights to create weighted percentiles, such as population-weighted quantiles. Unweighted if not specified. Vector same length as number of rows in data.frame.
#' @param na.rm Default is TRUE. Passed to [assign.pctiles.alt2()]
#' @param prefix Optional character element, default is 'pctile.', provides text to paste to beginning of input data.frame column names to use as output column names.
#' @param as.df Optional logical TRUE by default, in which case matrix results are converted to data.frame
#' @param zone NULL by default, but if a vector is provided, it defines zones to group by, so percentiles are within a given zone only.
#' @return Returns a matrix or data.frame
#' @template seePctiles
#' @export
make.pctile.cols.alt2 <- function(raw.data.frame, weights, as.df=TRUE, prefix='pctile.', na.rm=TRUE, zone=NULL) {

  #  alt2 FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE COLUMNS FOR MULTIPLE RAW DATA COLUMNS

  pctile.df <- sapply(raw.data.frame, FUN = function(x) {assign.pctiles.alt2(x, weights, na.rm = na.rm, zone = zone) })
	colnames(pctile.df) <- paste(prefix, colnames(pctile.df), sep = "")
	if (as.df) {pctile.df <- as.data.frame(pctile.df, stringsAsFactors = FALSE)}
	return(pctile.df)

	# Example of usage:
	#  new.pctile.cols <- make.pctile.cols.alt2(places[ , names.e])
}

