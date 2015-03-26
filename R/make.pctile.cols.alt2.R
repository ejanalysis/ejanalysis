make.pctile.cols.alt2 <- function(raw.data.frame, weights, as.df=TRUE, na.rm=TRUE, zone=NULL) {

  #  alt2 FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE COLUMNS FOR MULTIPLE RAW DATA COLUMNS
  
  pctile.df <- sapply(raw.data.frame, FUN=function(x) {assign.pctiles.alt2(x, weights, na.rm=na.rm, zone=zone) })
	colnames(pctile.df) <- paste("pctile.", colnames(pctile.df), sep="")
	if (as.df) {pctile.df <- as.data.frame(pctile.df, stringsAsFactors=FALSE)}
	return(pctile.df)

	# Example of usage:  
	#  new.pctile.cols <- make.pctile.cols.alt2(places[ , names.e])
}

