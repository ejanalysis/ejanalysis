write.wtd.pctiles.by.zone <- function(mydf, wts, filename, zone.vector) {

  # FUNCTIONS TO SHOW percentile STATS STRATIFIED BY REGION (OR STATE),
  #	SAVING A FILE OF STATS FOR EACH REGION OR STATE
  #  LIKE A SET OF LOOKUP TABLES

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
