#' @export
make.bin.pctile.cols <- function(raw.data.frame, weights=1, zone=NULL, as.df=TRUE, prefix.bin='bin.', prefix.pctile='pctile.', ...) {

  ###############################################################
  #	*** Example of usage of these functions to create multiple pctile and bin columns
  ###############################################################
  # **** this one function is a work in progress/ untested

  #this.as.df <- as.df; this.weights <- weights # this is one way to pass those parameters to the next functions if they say as.df=this.as.df, etc.
  pctile.df <- make.pctile.cols(raw.data.frame, weights=weights, zone=zone, as.df=as.df, prefix=prefix.pctile, ...)
  bin.df    <- make.bin.cols(pctile.df, as.df=as.df, prefix=prefix.bin, ...)
#  if (as.df) {
#    pctile.df <- as.data.frame(pctile.df)
#    bin.df    <- as.data.frame(bin.df)
#  }
  #  Need to test if all code below works with both as.df=TRUE and as.df=FALSE
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
    # test if cbind() results in the same speed and format as data.frame()
  }
}
