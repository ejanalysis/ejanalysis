#' @title Determine percentile a given value is at
#' @description Given a vector of numbers, and a single value of interest, 
#' determine what percentile that value is at. In other words, 
#' what fraction of all of the values are smaller than (or tied with) the single value of interest?
#' @param x 
#' @param values 
#' @param wts 
#' @param or.tied 
#' @param na.rm Logical, optional, TRUE by default. Should NA values (missing data) be removed first to get percentile of those with valid data.
#' @template seePctiles
#' @examples
#' get.pctile(89:95, 1:100)
#' get.pctile(89:95, 1:100, c(rep(1,90), rep(10000,10)))
#' @export
get.pctile <- function(x, values, wts, or.tied=TRUE, na.rm=TRUE) {

  # Find the (weighted) percentile of 1+ values, out of the full distribution of values
  # Returns 0 for x < min(values), and returns 1 for x >= max(values)
  # It returns what % of the sum of wts corresponds to values 

  # Using ecdf is another way to get this
  
  # Another way would be to use assign.pctiles and then return only those corresponding to x 

  # will write a vectorized version, but for now this works fine for getting a small number of percentiles:
  my.percentiles <- vector(length=length(x))
  for (i in 1:length(x)) {
    my.percentiles[i] <- pct.above(df=values, benchmarks=x[i], benchnames='x', below=TRUE, or.tied=or.tied, na.rm=na.rm, wts=wts)
  }
  
  # Another way: Could use this sapply( and sum(wts[  method that may be faster and is easier:
  #  if (missing(wts)) {wts <- rep(1, length(values))}
  #  if (length(wts)!=length(values)) {stop('Length of wts must equal length of values (cannot recycle wts)')}
  #  my.percentiles <- sapply(x, FUN=function(y) {sum(wts[y >= values], na.rm=na.rm) / sum(wts, na.rm=na.rm)})

  return(my.percentiles)
}