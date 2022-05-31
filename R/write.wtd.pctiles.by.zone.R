#' @title create lookup table as file of pop-weighted percentiles, mean, sd by state or region
#'
#' @param mydf data.frame with numeric data. Each column will be examined to calculate mean, sd, and percentiles, for each zone.
#' @param wts vector of numbers such as population counts as weights, as long as nrow(mydf)
#' @param filename prefix to use for filename to be saved locally
#' @param zone.vector names of states or regions, for example. same length as wts, or rows in mydf
#' @examples
#'   \dontrun{
#'   bg = ejscreen::bg21
#'   pctilevariables <- c(names.e, names.d, names.ej)
#'    ejanalysis::write.wtd.pctiles(mydf = bg[ , pctilevariables], wts = bg$pop, filename =  'lookupUSA')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$REGION, filename =  'lookupRegions')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$ST,     filename =  'lookupStates')
#'   }
#'
#' @export
write.wtd.pctiles.by.zone <- function(mydf, wts, filename, zone.vector) {

  # FUNCTIONS TO SHOW percentile STATS STRATIFIED BY REGION (OR STATE),
  #	SAVING A FILE OF STATS FOR EACH REGION OR STATE
  #  LIKE A SET OF LOOKUP TABLES

  # but now will fix it to just create a single table or file for the whole group of zones like all States in 1 csv file, as EJScreen would use.

  for (z in unique(zone.vector)) {
    r = data.frame(sapply(mydf[zone.vector==z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector==z]) ) )
    r = rbind(r, t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) Hmisc::wtd.mean(x, wts[zone.vector==z], na.rm=TRUE) ) ) ))
    r = rbind(r, t(data.frame(std.dev=sapply(mydf[zone.vector==z,  ], function(x) sqrt(Hmisc::wtd.var(x, wts[zone.vector==z], na.rm=TRUE) ) )) ))

    r
    write.csv(r, file=paste(filename, "-popwtd-for zone ", z, ".csv", sep=""))
        # could be too many to usefully return() here

  }
  # Example of usage:  write.wtd.pctiles.by.zone(places[ , names.e], places$pop, 'envt-data', places$REGION)
  # zone.vector should be a vector that is the data in the column to use for grouping, e.g. zone.vector <- places$REGION

}
