#' @title create lookup table as file of percentiles, mean, sd by state or region
#'
#' @description Given a data.frame, for each column in the data.frame,
#'   this function just returns percentiles, mean, and standard deviation.
#'   Also saves that as a csv file.
#' @param mydf data.frame with numeric data. Each column will be examined to calculate mean, sd, and percentiles, for each zone.
#' @param filename prefix to use for filename to be saved locally
#' @param zone.vector names of states or regions, for example. same length as wts, or rows in mydf
#' @seealso \link{write.wtd.pctiles.by.zone}
#' @export
write.pctiles.by.zone <- function(mydf, filename, zone.vector) {

  # FUNCTIONS TO SHOW percentile STATS STRATIFIED BY REGION (OR STATE),
  #	SAVING A FILE OF STATS FOR EACH REGION OR STATE
  #  LIKE A SET OF LOOKUP TABLES

  for (z in unique(zone.vector)) {
    r = data.frame(sapply(mydf[zone.vector==z, ], function(x) pctiles.exact(x) ))
    r = rbind(r, t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) mean(x, na.rm=TRUE) )) ))
    r = rbind(r, t(data.frame(std.dev=sapply(mydf[zone.vector==z, ], function(x) sd(x, na.rm=TRUE) )) ))
    r
    write.csv(r, file=paste(filename, "-notpopwtd-for zone ", z, ".csv", sep=""))
    # could be too many to usefully return() here
  }
  # zone.vector should be a vector that is the data in the column to use for grouping, e.g. zone.vector <- places$REGION

}
