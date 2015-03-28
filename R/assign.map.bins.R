#' @title Assign each place to a bin based on cutpoints
#'
#' @description Takes a vector of values and returns a vector of bin numbers.
#' For creating color-coded maps (choropleths), assign each place (e.g., each row of a single column) to a bin.
#' Each bin represents one map color, and is defined by cutoff values.
#' @details
#' The default bins 0-11 are defined as follows: \cr
#' bin 0: PCTILE=NA \cr
#' ... \cr
#' bin 9:  0.80<=PCTILE<0.90 \cr
#' bin 10: 0.90<=PCTILE<0.95 \cr
#' bin 11: 0.95<=PCTILE<=1.00  \cr
#' @param pctiles.vector Vector of percentiles as decimal fractions, 0 to 1, one per place.
#' @param cutpoints Optional vector of cutpoints defining edges of bins. Default is every 0.10 from 0 to 1.00, as well as 0.95
#' @param labels vector of bin numbers, optional (default is 1 through 11, and NA values are put in bin 0).
#' @return Returns a vector bin numbers.
#' @template seePctiles
#' @examples
#' junk<-c(0,0.799,0.8, 0.8000001, 0.8999999,0.95,0.95001,1)
#' data.frame(pctile=junk, bin=assign.map.bins(junk))
#' # How it puts these in bins by default:
#' #      pctile bin   notes
#' #1  0.0000000   1
#' #2  0.7990000   8
#' #
#' #3  0.8000000   9 (0.80<=PCTILE<0.90 )
#' #4  0.8000001   9 (0.80<=PCTILE<0.90 )
#' #5  0.8999999   9 (0.80<=PCTILE<0.90 )
#' #
#' #6  0.9000000  10 (0.90<=PCTILE<0.95 )
#' #7  0.9001000  10 (0.90<=PCTILE<0.95 )
#' #8  0.9499990  10 (0.90<=PCTILE<0.95 )
#' #
#' #9  0.9500000  11 (0.95<=PCTILE<=1.00 ) I.E. THIS INCLUDES 100th percentile
#' #10 0.9500100  11 (0.95<=PCTILE<=1.00 ) I.E. THIS INCLUDES 100th percentile
#' #11 1.0000000  11 (0.95<=PCTILE<=1.00 ) I.E. THIS INCLUDES 100th percentile
#' @export
assign.map.bins <- function(pctiles.vector, cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11) {

  #  FUNCTION TO ASSIGN MAP BIN NUMBER 1-11, BASED ON THESE %ILE CUTPOINTS: defaults are c( (0:9)/10, 0.95, 1)

  # Inputs must be fractions 0-1, not integers 0-100, if default fractional cutpoints are used.
  mylabels <- labels

  bin.vector <- cut(pctiles.vector, breaks=cutpoints, labels=mylabels, right=FALSE, include.lowest=TRUE)
	bin.vector <- as.numeric(bin.vector)

	# Put NA values in a bin zero (i.e., where missing data in inputs so percentile is NA)

	bin.vector[is.na(bin.vector)] <- 0
	return(bin.vector)
}
