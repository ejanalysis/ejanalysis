#' @title Make columns of (weighted) percentiles and also bin numbers, from columns of values
#'
#' @description This function just combines \code{\link{make.pctile.cols}} and \code{\link{make.bin.cols}}.
#' Takes a data.frame of values and returns a data.frame of percentiles,
#' showing the percentile of a value within all values in its column, as well as bin numbers,
#' showing what bin each falls into, based on specified cutoffs. \cr\cr
#' ** Work in progress/ not fully tested, e.g., need to test if all code below works with both as.df=TRUE and as.df=FALSE
#' @param raw.data.frame Data.frame of values
#' @param weights Optional Numeric vector of weights to create weighted percentiles, such as population-weighted quantiles. Unweighted if not specified. Vector same length as number of rows in data.frame.
#' @param prefix.pctile Optional character element, default is 'pctile.', provides text to paste to beginning of input data.frame column names to use as pctile output column names.
#' @param prefix.bin Optional character element, default is 'bin.', provides text to paste to beginning of input data.frame column names to use as bin output column names.
#' @param as.df Optional logical TRUE by default, in which case matrix results are converted to data.frame
#' @param zone NULL by default, but if a vector is provided, it defines zones to group by, so percentiles are within a given zone only.
#' @return Returns a matrix or data.frame
#' @template seePctiles
#' @export
make.bin.pctile.cols <- function(raw.data.frame, weights=1, zone=NULL, as.df=TRUE, prefix.bin='bin.', prefix.pctile='pctile.', ...) {
  #this.as.df <- as.df; this.weights <- weights # this is one way to pass those parameters to the next functions if they say as.df=this.as.df, etc.
  pctile.df <- make.pctile.cols(raw.data.frame, weights=weights, zone=zone, as.df=as.df, prefix=prefix.pctile, ...)
  bin.df    <- make.bin.cols(pctile.df, as.df=as.df, prefix=prefix.bin, ...)
#  if (as.df) {
#    pctile.df <- as.data.frame(pctile.df)
#    bin.df    <- as.data.frame(bin.df)
#  }

  rownames(pctile.df) <- NULL; rownames(bin.df) <- NULL # otherwise 100* pctile.df won't work!
  pctile.df <- 100 * pctile.df   # Note those are exact percentiles 0-100, not rounded or floored

  #  If wanted to get floored pctiles as well:
  #floor.pctiles <- data.frame(lapply(pctile.df, floor)) # rounds down so that 79.99%ile doesn't display as 80th since it is truly<80th
  #names(floor.pctiles) <- gsub(prefix.pctile, paste('floor', prefix.pctile, sep=''), names(floor.pctiles))
  #names(pctile.df) <- gsub('pctile.', prefix.pctile, names(pctile.df)) # this fixes prefix if old make.pctile.cols didn't already
  #names(bin.df) <- gsub('bin.', prefix.bin, names(bin.df)) # this fixes prefix if old make.bin.cols didn't already

  if (as.df) {
    return(data.frame(pctile.df, bin.df, stringsAsFactors=FALSE) )
    } else {
    return(cbind(pctile.df, bin.df) )
  }
}
