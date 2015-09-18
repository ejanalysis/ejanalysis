#' @title Overlay Two PDFs as Weighted Histogram-Like Density Plots
#' @description Use plot(density()) to see overlay of two distribution functions. 
#' @details Assumes you have weights for each and are comparing values in one group vs another.
#' @param e Environmental or other indicator values vector
#' @param dcount Vector of weights for the demographic group of interest, such as population counts of Hispanics by Census tract.
#' @param refcount Vector of weights for the reference group, such as population counts of individuals who are not Hispanic, by Census tract.
#' @param etxt Character string to name e in graph
#' @param dtxt Character string to name d in graph
#' @param brks Default is 10. Passed as breaks param to plot function
#' @param ... other parameters passed to \code{density}
#' @return Creates a plot
#' @seealso \code{\link{pop.cdf}}   \code{\link{pop.cdf2}} \code{\link{pop.ecdf}}  \code{\link{pop.cdf.density}} 
#' @examples 
#' \donotrun{
#'   pop.cdf.density(e = e, dcount = dcount, refcount = refcount, etxt = etxt, dtxt = dtxt, 
#                adjust=2, brks = brks)
#' }
#' @export
pop.cdf.density <- function(e, dcount, refcount, etxt, dtxt, brks=10, ...) {
  plot(density(e, weights=refcount/sum(refcount,na.rm=TRUE), ...), col='gray', ylab='Density (percentage of group population)',
       xlab=etxt, breaks=brks, 
       main=paste(etxt, " distribution within each group
                  (blue=", dtxt, ", gray=reference group)", sep=''))
  lines(density(e, weights=dcount/sum(dcount,na.rm=TRUE), ...), col='blue') 
}
