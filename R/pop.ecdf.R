#' @title Draw an Ecdf plot comparing distributions of scores in selected demographic groups
#'
#' @description
#'  Draws a plot using \code{\link[Hmisc]{Ecdf}}, overlaying cumulative distribution functions,
#'  one for each subgroup specified.
#'  Useful to compare 2 groups based on each groups entire pdf or cdf  distribution of peoples scores,
#'  using data from small places like census block groups, based on having for each place the pop total
#'  and % of pop that is in each group or perhaps already have count in each group.
#' @details
#' Notes: \cr
#' to compare zones, \cr
#' compare demog groups, (see parameter called group)\cr
#' compare multiple groups and/or multiple zones, like hisp vs others in us vs ca all on one graph\cr
#' see \code{\link{Ecdf}} for options & try passing a data.frame instead of just vector\cr
#' #' \cr\cr
#' @param scores Numeric vector, required. Values to analyze.
#' @param pcts Numeric vector or data.frame, required. Same number of vector elements or data.frame
#'   rows as length of scores. Specifies the fraction of population that is in demographic group(s) of interest, one row per place.
#' @param pops ***
#' @param allothers Logical value, optional, TRUE by default. ***
#' @param col Optional, default is 'red' to signify line color red for key demographic group
#' @param main Required character specifying plot title
#' @param weights Vector of weights for weighted frequency distributions
#' @param subtitles Logical FALSE by default
#' @param ... other optional parameters to pass to Ecdf
#' @return Same as \code{\link[Hmisc]{Ecdf}} -- draws a plot
#' @seealso \code{\link[Hmisc]{Ecdf}} \code{\link{RR}}
#' @examples
#' ###
#' \dontrun{
#' # pop.ecdf( 31:35, c(0.10, 0.10, 0.40, 0, 0.20), 1001:1005 )
#' # pop.ecdf(dat$Murder, dat$Population * (dat$Illiteracy/100))
#' #
#' # pop.ecdf(places$pm, places$pctmin, places$pop)
#' # pop.ecdf(log10(places$traffic.score), places$pctmin, places$pop)
#' # pop.ecdf(places$cancer, places$pctmin, places$pop, allothers=FALSE); pop.ecdf(places$cancer, places$pctlingiso, places$pop, col='green', allothers=FALSE, add=TRUE)
#' # Demog suscept  for each REGION (can't see if use vs others)
#' pop.ecdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          group=bg$REGION, allothers=FALSE,
#'          xlab='Traffic score (log scale)', ylab='%ile of population', main='Distribution of scores by EPA Region')
#'
#' # Demog suscept (how to show vs others??), one panel per REGION
#' pop.ecdf(bg[ , names.e], bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          allothers=TRUE, ylab='%ile of population', main='Distribution of scores by EPA Region')
#'
#' # log scale is useful & so are these labels passed to function
#' # in CA vs not CA
#' pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop,
#'          subtitles=FALSE,
#'          log='x', xlab='%ile of population', ylab='Traffic scores (log scale)',
#'          main='Distribution of scores in CA (red) vs rest of US')
#'
#' # Flagged vs not (all D, all zones)
#' pop.ecdf(bg$traffic.score, bg$flagged, bg$pop, log='x')
#'
#' # D=Hispanics vs others, within CA zone only
#' pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$pcthisp, log='x')
#' # Demog suscept vs others, within CA only
#' pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$VSI.eo, log='x')
#'
#' }
#' @export
pop.ecdf <- function(scores, pcts, pops, allothers=TRUE, col='red', main, weights, subtitles=FALSE, ...) {

	# should add error checking here

  if (missing(main)) {main <- paste('ECDF of scores in selected group (', col, ') ', ifelse(allothers, 'vs. rest of the population',''), sep='')}
  if (missing(pops)) {pops <- 1}
  if (!missing(weights)) {warning('weights parameter is currently ignored, since pops*(1-pcts) is used as weights now')}
  #require(Hmisc)

  if (is.vector(pcts)) {
    Ecdf(scores, weights=(pcts * pops), main=main, col=col, subtitles=subtitles, ...)
    if (allothers) { Ecdf(scores, weights=((1 - pcts) * pops), add=TRUE, col='black', subtitles=FALSE, ...) }
    # note can't draw subtitles (n, m) once for ref group and once for nonref group in same spot - the text would be overwritten and look wrong
  }

  if (is.data.frame(pcts)) {
    # plot one cdf per col of df
    # can't pass df to Ecdf because that tries to draw one cdf per panel, multiple panels
    # can't use groups param in Ecdf, since that assumes each group is a unique set of rows, but we need different weighting for each group to be estimated
    for (i in 1:length(scores[1,]))
    Ecdf(scores, weights=(pcts * pops), main=main, col=col, subtitles=subtitles, ...)
  }
}
