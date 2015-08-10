write.pctiles <- function(mydf, filename) {
  # FUNCTION TO WRITE CSV FILE WITH PERCENTILES AND MEAN, AND MAYBE STD DEVIATION,
  # LIKE A LOOKUP TABLE
  r <- data.frame(sapply(mydf, function(x) pctiles.exact(x) ))
  r <- rbind(r, t(data.frame(mean=sapply(mydf, function(x) mean(x, na.rm=TRUE) )) ))
  r <- rbind(r, t(data.frame(std.dev=sapply(mydf, function(x) sd(x, na.rm=TRUE) )) ))
  r
  write.csv(r, file=paste(filename, ".csv", sep=""))
  return(r)
}
