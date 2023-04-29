#' @title Margin of Error for a Sum
#'
#' @description
#'   Estimates the margin of error (MOE) that characterizes uncertainty, for a sum of estimates,
#'   based on their MOE values.
#'
#' @details
#'   This is based on US Census Bureau recommendations for working with American Community Survey summary file data.
#'   This also works for differences (subtraction), not just sums.
#'   For example, one can estimate MOE for number of people with less than high school education in a block group, given
#'   estimates and margins of error for the count whose educational attainment is no school, first grade, second grade... 11th grade.
#'
#' @param estimates A matrix or data.frame of numbers that are the estimates to be added across columns.
#'   Each row represents another place such as a Census block group, where the sum of all columns is calculated once for each place.
#' @param moes A matrix of data.frame like estimates parameter, but with MOE values for the corresponding counts provided in estimates.
#' @param include0 Default is FALSE. If TRUE, based MOE on all data, even where estimate is zero, which gives MOEs that create conservatively wide confidence intervals.
#' @return Returns margin(s) of error same shape as parameter estimates
#' @seealso [pct.moe()]
#' @examples
#'  povknownratio <- c(500, 2000, 1500); povknownratio.m <- c(100, 300, 250)
#'  pov2plus <- c(300, 1000, 1400); pov2plus.m <- c(100, 300, 200)
#'  lowinc <- povknownratio - pov2plus
#'  lowinc.m <- sumoe(cbind(povknownratio, pov2plus), cbind(povknownratio.m, pov2plus.m))
#'  cbind(lowinc=lowinc, MOE=lowinc.m, PCTMOE=round(lowinc.m/lowinc,2))
#' @export
sumoe <- function(estimates, moes, include0=FALSE) {
  if (include0) {
    return( sqrt( rowSums(moes^2)) )
  } else {
    moesifzeroest    <- ifelse(estimates==0, moes, 0)
    moesifnonzeroest <- ifelse(estimates!=0, moes, 0)
    singlemoetouse <- apply(moesifzeroest, 1, max)
    Result <- sqrt( rowSums(moesifnonzeroest^2) + singlemoetouse^2)
    return( Result )
  }

}
