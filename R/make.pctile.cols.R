#' @title Make columns of (weighted) percentiles from columns of values
#'
#' @description Takes values (vector, matrix, or data.frame) and returns percentiles,
#'   showing the percentile of a value within all values in its column.
#' @param values Data.frame, matrix, or vector of values. One column at a time is analyzed using [assign.pctiles()]
#' @param weights Optional numeric vector of weights (default is unweighted) to create weighted percentiles, such as population-weighted quantiles. Unweighted if not specified. Vector same length as number of rows in values parameter (or elements if vector).
#' @param prefix Optional character element, default is 'pctile.', provides text to paste to beginning of input data.frame column names to use as output column names.
#' @param as.df Optional logical TRUE by default, in which case matrix results are converted to data.frame
#' @param zone NULL by default, but if a vector is provided, it defines zones to group by, so percentiles are within a given zone only.
#' @return Returns a matrix or data.frame
#' @seealso [make.bin.pctile.cols()] and [assign.pctiles()]
#' @export
make.pctile.cols <- function(values, weights=1, prefix='pctile.', as.df=TRUE, zone=NULL) {

  #  FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE COLUMNS FOR MULTIPLE RAW DATA COLUMNS
  if (is.vector(values)) {
    pctile.df <- as.matrix( assign.pctiles(values, weights = weights, zone=zone) )
  } else {
    pctile.df <- apply(values, MARGIN=2, FUN=function(x) {assign.pctiles(x, weights=weights, zone=zone) })
  }
	colnames(pctile.df) <- paste(prefix, colnames(pctile.df), sep="")
	if (as.df) {pctile.df <- as.data.frame(pctile.df, stringsAsFactors=FALSE)}
	return(pctile.df)

	# Example of usage:
	#  new.pctile.cols <- make.pctile.cols(bg[ , names.e], bg$pop)  # for pop weighted percentiles
}
