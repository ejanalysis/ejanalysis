#' @title Create a Bin Numbers Column for each Percentiles Column
#'
#' @description
#'   Simplifies creation of data.frame or matrix (or vector) with columns that specify bins by bin number,
#'   based on values (percentiles for example) and specified cutpoints. Each bin is defined by cutpoints.
#' @details
#'   This is one way to prepare data to be mapped in a series of choropleths, for example (color-coded maps).
#'
#' The default bins 0-11 are defined as follows: \cr
#' bin 0: PCTILE=NA \cr
#' ... \cr
#' bin 9:  0.80<=PCTILE<0.90 \cr
#' bin 10: 0.90<=PCTILE<0.95 \cr
#' bin 11: 0.95<=PCTILE<=1.00  \cr
#' @param pctile.df Data.frame (or matrix or vector), required. Typically percentiles as decimal fractions, 0 to 1, one per place.
#' @param as.df Logical value, optional, TRUE by default. Defines whether results are data.frame or matrix.
#' @param cutpoints Optional vector of cutpoints defining edges of bins. Default is every 0.10 from 0 through 1.00, as well as 0.95
#' @param labels Vector of bin numbers (defining what is returned for values in those bins), optional (default is 1 through 11, and NA values are put in bin 0).
#' @param prefix Optional character element, ".bin" by default, pasted as prefix to each column name of pctile.df, and used as names of returned columns.
#' @return Returns a data.frame, matrix, or vector (depending on `as.df`) the same shape as pctile.df
#' @seealso [make.bin.pctile.cols()] and [assign.pctiles()]
#' @examples
#' 	#  new.bin.cols <- make.bin.cols(places[ , names.e.pctile])
#' 	# new.bin.cols <- make.bin.cols( new.pctile.cols )
#' @export
make.bin.cols <- function(pctile.df, as.df=TRUE, cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11, prefix='bin.') {
  #bin.df <- sapply(pctile.df, FUN=function(x) {assign.map.bins(x, cutpoints, labels) })
  if (is.vector(pctile.df)) {
    bin.df <- as.matrix( assign.map.bins(pctile.df, cutpoints, labels) )
  } else {
    bin.df <- apply(pctile.df, MARGIN=2, FUN=function(x) {assign.map.bins(x, cutpoints, labels) })
  }
	colnames(bin.df) <- paste(prefix, colnames(bin.df), sep="")
	colnames(bin.df) <- gsub('bin.pctile.', 'bin.', colnames(bin.df))  # get rid of pctile. in field names, if created by make.pctile.cols()
	if (as.df) {bin.df <- as.data.frame(bin.df, stringsAsFactors=FALSE)}
	return(bin.df)
}
