#' @seealso \code{\link{make.bin.pctile.cols}} to call make.pctile.cols and make.bin.cols \cr
#' \code{\link{assign.pctiles}} to assign percentile (quantile) to each value within its column, in data.frame  \cr
#' \code{\link{assign.pctiles.alt2}} for alternative method to replicate assign.pctiles \cr
#' \code{\link{get.pctile}} to get (weighted) percentile of 1+ values within given vector of values \cr
#' \code{\link{assign.map.bins}} to assign map color bin using breaks to each value in data.frame given its percentile \cr
#' \code{\link{make.pctile.cols}} to take df and return same-sized df that is wtd.quantile of each value within its column \cr
#' \code{\link{make.pctile.cols.alt2}} to use alt version of make.pctile.cols for replication \cr
#' \code{\link{make.bin.cols}} to take df of pctile values and return same-sized df that is bin number (map color bin) using preset breaks. \cr
#' \code{\link{write.pctiles}} to save file that is lookup table of percentiles for columns of a data.frame \cr
#' \code{\link{write.pctiles.by.zone}} to save file that is lookup table of percentiles for columns of a data.frame, for each geographic zone (subset of rows) \cr
#' \code{\link{write.wtd.pctiles}} to save file that is lookup table of weighted percentiles for columns of a data.frame \cr
#' \code{\link{write.wtd.pctiles.by.zone}} to save file that is lookup table of weighted percentiles for columns of a data.frame, for each geographic zone (subset of rows) \cr
#' \code{\link{lookup.pctile}} to look up current approx weighted percentiles in a lookup table that is already in global memory \cr
