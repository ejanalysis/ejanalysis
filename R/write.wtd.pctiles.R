write.wtd.pctiles <- function(mydf, wts, filename) {
  # FUNCTION TO WRITE CSV FILE WITH PERCENTILES AND MEAN, AND MAYBE STD DEVIATION,
  # LIKE A LOOKUP TABLE
  r = data.frame(sapply(mydf, function(x) wtd.pctiles.exact(x, wts) ) )
  r = rbind(r, t(data.frame(mean=sapply(mydf, function(x) wtd.mean(x, wts, na.rm=TRUE) ) ) ))
  r = rbind(r, t(data.frame(std.dev=sapply(mydf, function(x) sqrt(wtd.var(x, wts, na.rm=TRUE)) ) ) ))
  r
  write.csv(r, file=paste(filename, ".csv", sep=""))
  return(r)
  # Example of usage:  write.wtd.pctiles(places[ , names.e], places$pop, 'envt-data')
}
