write.RR.tables <- function(my.RR.table, folder=getwd()) {

  # FUNCTION TO TAKE OUTPUT OF RR.table() FUNCTION, AND 
  # WRITE ONE CSV FILE FOR EACH GEOGRAPHIC ZONE, 
  # WHERE EACH CSV FILE IS THE TABLE OF RELATIVE RISK VALUES, ONE DEMOGRAPHIC GROUP PER ROW AND ONE ENVIRONMENTAL INDICATOR PER COLUMN, PLUS MAX PER ROW AND MAX PER COLUMN
  
  tabname <- deparse(substitute(my.RR.table))
  zonecount <- length(my.RR.table[1,1,])
  filename <- vector(NA, length=zonecount)

  for (i in 1:zonecount) {
    filename[i] <- file.path(folder, paste(tabname, '.', dimnames(my.RR.table)[[3]][i],'.csv' , sep=''))
    
    # You can't really sort cols or rows within each zone, unless just for purposes of saving each zone as a separate csv file, 
    # because then that will make each zone have different sort order to its rows and cols, so they don't align across zones, which won't make sense for the 3-D array structure.

    sorted.zone <- my.RR.table[ , , i] # focus on just this zone
    sorted.zone   <- sorted.zone[order(sorted.zone[ , 1]), ] # reorder rows based on first column
    sorted.zone <- sorted.zone[ , order(sorted.zone[1, ])]  # reorder cols based on first row
    
    write.csv(sorted.zone, file=filename, row.names=TRUE)
  }

  return(filename)
  # Example
  RRS.REGION <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='REGION')
  write.RR.tables(RRS.REGION)
}
