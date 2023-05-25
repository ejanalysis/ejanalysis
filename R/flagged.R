#' @title Which rows have a value above threshold in at least one column
#'
#' @description Flags rows that have values above some threshold in at least one column.
#' @details The use of na.rm=TRUE in this function means it will always ignore NA values in a given place and take the max of the valid (non-NA) values instead of returning NA when there is an NA in that row
#' @param df Data.frame with numeric values to be checked against the threshold.
#' @param threshold Number that is the threshold that must be (met or) exceeded for a row to be flagged. Optional, default is 0.80
#' @param or.tied Logical, optional, default is TRUE, in which case a value equal to the threshold also flags the row.
#' @return Returns a logical vector or data.frame the shape of df
#' @examples
#' set.seed(999)
#' places <- data.frame(p1=runif(10, 0,1), p2=c(NA, runif(9,0,1)), p3=runif(10,0,1))
#' pctilecols <- c('p1','p2', 'p3')
#' x <- flagged(places[ , pctilecols], 0.80)
#' a <- cbind(any.over.0.8=x, round(places,2))
#' a[order(a[,1]),]
#' @export
flagged <- function(df, threshold=0.80, or.tied=TRUE) {

	# CREATE A LOGICAL VECTOR THAT IS TRUE FOR EACH ROW OF DATA FRAME WHERE AT LEAST ONE VALUE IN ROW IS >= threshold (or just >threshold if above.only=TRUE)

  # *** Be careful to check if percentiles are 0-100 or 0-1 !!! ***
  if (threshold <= 1 & any(df > 1, na.rm = TRUE) ) {warning('threshold is <=1 so it might be a percentage as fraction, but some of data are >1 so may percentages as 0-100 not as fraction')}

	if (or.tied) {
    flag <- do.call(pmax, c(df, na.rm=TRUE)) >= threshold
	} else {
    flag <- do.call(pmax, c(df, na.rm=TRUE)) > threshold
	}
	return(flag)
}

