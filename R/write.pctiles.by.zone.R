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
