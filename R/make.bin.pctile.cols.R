#' @title Weighted Percentiles and Bin Numbers for Each Column, by zone, such as percentiles within each State
#'
#' @description This function just combines [make.pctile.cols()] and [make.bin.cols()].
#' Takes a data.frame of values and returns a data.frame (or matrix) of percentiles,
#' showing the percentile of a value within all values in its column, as well as bin numbers,
#' showing what bin each falls into, based on specified cutpoints defining bins. \cr\cr
#' ** Work in progress/ not fully tested, e.g., need to test if all code below works with both as.df=TRUE and as.df=FALSE
#' @param raw.data.frame Data.frame of values
#' @param weights Optional Numeric vector of weights to create weighted percentiles, such as population-weighted quantiles. Unweighted if not specified. Vector same length as number of rows in data.frame.
#' @param prefix.pctile Optional character element, default is 'pctile.', provides text to paste to beginning of input data.frame column names to use as pctile output column names.
#' @param prefix.bin Optional character element, default is 'bin.', provides text to paste to beginning of input data.frame column names to use as bin output column names.
#' @param as.df Optional logical TRUE by default, in which case matrix results are converted to data.frame
#' @param zone NULL by default, but if a vector is provided, it defines zones to group by, so percentiles are within a given zone only.
#' @param cutpoints Default is 1:11. see [make.bin.cols()]
#' @param labels Default is c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90,  0.95,  1.00). see [make.bin.cols()]
#' @return Returns a matrix or data.frame
#'
#' @seealso
#' [make.bin.pctile.cols()] to call functions below, converting columns of values to percentiles and then bins
#' [assign.pctiles()] for one vector, assign (weighted) percentile (quantile) to each value within its zone (subset)
#' [assign.pctiles.alt2()] as an alternative method, to replicate assign.pctiles, but not by zone
#' [get.pctile()] to get (weighted) percentile of just 1+ values within given vector of values
#' [make.pctile.cols()] for a data.frame, assign percentiles, return a same-sized df that is wtd.quantile of each value within its column
#' [make.pctile.cols.alt2()] as an alternative method, to replicate make.pctile.cols
#' [assign.map.bins()] for one vector (or data.frame) of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks.
#' [make.bin.cols()] for a data.frame of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks.
#' [write.pctiles()] to save file that is lookup table of percentiles for columns of a data.frame
#' [write.pctiles.by.zone()] to save file that is lookup table of percentiles for columns of a data.frame, for each geographic zone (subset of rows)
#' [write.wtd.pctiles()] to save file that is lookup table of weighted percentiles for columns of a data.frame
#' [write.wtd.pctiles.by.zone()] to save file that is lookup table of weighted percentiles for columns of a data.frame, for each geographic zone (subset of rows)
#' [lookup.pctile()] to look up current approx weighted percentiles in a lookup table that is already in global memory
#'
#' @export
#'
make.bin.pctile.cols <- function(raw.data.frame, weights=NULL, zone=NULL, as.df=TRUE, prefix.bin='bin.', prefix.pctile='pctile.', cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11) {

    #this.as.df <- as.df; this.weights <- weights # this is one way to pass those parameters to the next functions if they say as.df=this.as.df, etc.
  pctile.df <- make.pctile.cols(raw.data.frame, weights=weights, zone=zone, as.df=as.df, prefix=prefix.pctile)
  bin.df    <- make.bin.cols(pctile.df, as.df=as.df, prefix=prefix.bin, cutpoints=cutpoints, labels=labels)
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
