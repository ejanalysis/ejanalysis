make.pctile.cols <- function(raw.data.frame, weights=1, prefix='pctile.', as.df=TRUE, zone=NULL) {

  #  FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE COLUMNS FOR MULTIPLE RAW DATA COLUMNS
  
  pctile.df <- sapply(raw.data.frame, FUN=function(x) {assign.pctiles(x, weights, zone=zone) })
	colnames(pctile.df) <- paste(prefix, colnames(pctile.df), sep="")
	if (as.df) {pctile.df <- as.data.frame(pctile.df, stringsAsFactors=FALSE)}
	return(pctile.df)

	# Example of usage:  
	#  new.pctile.cols <- make.pctile.cols(bg[ , names.e], bg$pop)  # for pop weighted percentiles 
}
