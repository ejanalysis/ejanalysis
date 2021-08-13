#' create lookup table as file of pop-weighted percentiles, mean, std.dev
#'
#' @param mydf data.frame with numeric data. Each column will be examined to calculate mean, sd, and percentiles
#' @param wts vector of numbers such as population counts as weights, as long as nrow(mydf)
#' @param filename prefix to use for filename to be saved locally
#' @examples
#'   \dontrun{
#'   write.wtd.pctiles(bg20[ , names.e], wts = bg20$pop, filename = 'envt-data')
#'   }
#'
#' @export
write.wtd.pctiles <- function(mydf, wts, filename) {
  # FUNCTION TO WRITE CSV FILE WITH PERCENTILES AND MEAN, AND MAYBE STD DEVIATION,
  # LIKE A LOOKUP TABLE
  r = data.frame(sapply(mydf, function(x) wtd.pctiles.exact(x, wts) ) )
  r = rbind(r, t(data.frame(mean=sapply(mydf, function(x) Hmisc::wtd.mean(x, wts, na.rm=TRUE) ) ) ))
  r = rbind(r, t(data.frame(std.dev=sapply(mydf, function(x) sqrt(Hmisc::wtd.var(x, wts, na.rm=TRUE)) ) ) ))
  r
  write.csv(r, file=paste(filename, ".csv", sep=""))
  return(r)
  # Example of usage:  write.wtd.pctiles(bg20[ , names.e], bg20$pop, 'envt-data')
}
