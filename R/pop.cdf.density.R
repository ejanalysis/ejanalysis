#' @title Overlay 2 PDFs as Weighted Histograms
#' @description OK but there now are better ways to compare pdf plots
#' @details  For an easy, nice, smoothed density plot,
#'   try plot(stats::density(bw = .001)); points(density(, bw = .001), col="red")
#'   to see overlay of two frequency distributions.
#'   Assumes you have weights for each and are comparing values in one group vs another.
#' @param e Environmental or other indicator values vector
#' @param dcount Vector of weights for the demographic group of interest, such as population counts of Hispanics by Census tract.
#' @param refcount Vector of weights for the reference group, such as population counts of individuals who are not Hispanic, by Census tract.
#' @param etxt Character string to name e in graph
#' @param dtxt Character string to name d in graph
#' @param brks Default is 10. Passed as breaks param to plot function
#' @param ... other parameters passed to \code{plot} # used to pass to density() but this is more useful
#' @return Creates a plot
#' @seealso \code{\link{pop.cdf}}   \code{\link{pop.cdf2}} \code{\link{pop.ecdf}}  \code{\link{pop.cdf.density}}
#' @examples
#' \dontrun{
#'   bg <- ejscreen::bg22[, c(ejscreen::names.d, 'pop', ejscreen::names.e, 'REGION')]
#'
#' e <- bg$pm[!is.na(bg$pm)]
#' dpct <- bg$pctmin
#' dcount   <- bg$pop[!is.na(bg$pm)] *      dpct[!is.na(bg$pm)]
#' refcount <- bg$pop[!is.na(bg$pm)] * (1 - dpct[!is.na(bg$pm)])
#' brks <- 0:17
#' etxt <- 'PM2.5'
#' dtxt <- 'Minorities'
#'
#' pop.cdf(        e, pcts = dpct, pops = bg$pop)
#' pop.cdf2(       e, dcount, refcount, etxt, dtxt, brks)
#' pop.cdf.density(e, dcount, refcount, etxt, dtxt )
#'
#'   pop.cdf.density(e = e, dcount = dcount, refcount = refcount, etxt = etxt, dtxt = dtxt,
#'                adjust=2, brks = brks)
#' }
#' @export
pop.cdf.density <- function(e, dcount, refcount, etxt, dtxt, brks=10, ...) {
  # remove places where e is NA
  ok <- !is.na(e)
  e <- e[ok]; dcount=dcount[ok]; refcount=refcount[ok]

  plot(density(e, weights=refcount / sum(refcount,na.rm=TRUE)), ..., col='gray', ylab='Density (percentage of group population)',
       xlab=etxt, breaks=brks,
       main=paste0(etxt, " distribution within each group"))
  lines(density(e, weights=dcount / sum(dcount, na.rm=TRUE), ...), col='darkblue')
  legend(x = "right", legend = c("Reference", dtxt), fill = c("gray", "darkblue"))

}
