if (1==0) {
  
  # NOTES/ WORK IN PROGRESS
  
  # install.packages('data.table')
  require(data.table)
  
  # aggregate.data.table <- etc. below
  # ADDS A data.table METHOD TO aggregate()  ???? But it already has one???
  # so that when you pass a data.table to aggregate() now, 
  # it will use this function, not the default aggregate()
  #
  
  aggregate.data.table <- function(x, by, FUN=mean, ..., is.value=is.numeric) {
    value_columns <- names(x)[which(sapply(x, is.value))]
    x[,lapply(.SD,FUN,...),eval(substitute(by)),.SDcols=value_columns]
  }
  
  bgt <- data.table(bg, key='FIPS.TRACT')
  
  x=aggregate(bgt, by=list(bg$FIPS.TRACT))
  
}
