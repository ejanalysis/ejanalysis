make.bin.cols <- function(pctile.df, as.df=TRUE, cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11, prefix='bin.') {

  #  FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE bin number COLUMNS FOR MULTIPLE percentile COLUMNS
  
  bin.df <- sapply(pctile.df, FUN=function(x) {assign.map.bins(x, cutpoints, labels) })
	colnames(bin.df) <- paste(prefix, colnames(bin.df), sep="")
	colnames(bin.df) <- gsub('bin.pctile.', 'bin.', colnames(bin.df))  # get rid of pctile. in field names, if created by make.pctile.cols()
	if (as.df) {bin.df <- as.data.frame(bin.df, stringsAsFactors=FALSE)}
	return(bin.df)
	
	# Example of usage:  
	#  new.bin.cols <- make.bin.cols(places[ , names.e.pctile])
	# or 
	#  new.bin.cols <- make.bin.cols( new.pctile.cols ) 
}