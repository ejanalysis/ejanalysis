#' @seealso
#' \code{\link{make.bin.pctile.cols}} to call functions below, converting columns of values to percentiles and then bins \cr
#' \cr
#' \code{\link{assign.pctiles}} for one vector, assign (weighted) percentile (quantile) to each value within its zone (subset)  \cr
#' \code{\link{assign.pctiles.alt2}} as an alternative method, to replicate assign.pctiles, but not by zone \cr
#' \code{\link{get.pctile}} to get (weighted) percentile of just 1+ values within given vector of values \cr
#' \cr
#' \code{\link{make.pctile.cols}} for a data.frame, assign percentiles, return a same-sized df that is wtd.quantile of each value within its column \cr
#' \code{\link{make.pctile.cols.alt2}} as an alternative method, to replicate make.pctile.cols \cr
#' \cr
#' \code{\link{assign.map.bins}} for one vector (or data.frame) of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks. \cr
#' \cr
#' \code{\link{make.bin.cols}} for a data.frame of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks. \cr
#' \cr
#' \code{\link{write.pctiles}} to save file that is lookup table of percentiles for columns of a data.frame \cr
#' \code{\link{write.pctiles.by.zone}} to save file that is lookup table of percentiles for columns of a data.frame, for each geographic zone (subset of rows) \cr
#' \code{\link{write.wtd.pctiles}} to save file that is lookup table of weighted percentiles for columns of a data.frame \cr
#' \code{\link{write.wtd.pctiles.by.zone}} to save file that is lookup table of weighted percentiles for columns of a data.frame, for each geographic zone (subset of rows) \cr
#' \cr
#' \code{\link{lookup.pctile}} to look up current approx weighted percentiles in a lookup table that is already in global memory \cr
