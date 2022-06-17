#' @title create lookup table as file of pop-weighted percentiles, mean, std.dev
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

  write.wtd.pctiles.by.zone(mydf=mydf, wts=wts, filename=filename)
  # FUNCTION TO WRITE CSV FILE WITH PERCENTILES AND MEAN, AND MAYBE STD DEVIATION,
  # LIKE A LOOKUP TABLE
   # Example of usage:  write.wtd.pctiles(bg20[ , names.e], bg20$pop, 'envt-data')
}
