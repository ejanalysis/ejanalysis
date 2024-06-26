#' @title Draw an Ecdf plot comparing distributions of scores in selected demographic groups
#'
#' @description
#'  Draws a plot using [Hmisc::Ecdf()], overlaying cumulative distribution functions,
#'  one for each subgroup specified.
#'  Useful to compare 2 groups based on each groups entire pdf or cdf  distribution of peoples scores,
#'  using data from small places like census block groups, based on having for each place the pop total
#'  and \% of pop that is in each group or perhaps already have count in each group.
#' @details
#' Notes: \cr
#' to compare zones, \cr
#' compare demog groups, (see parameter called group)\cr
#' compare multiple groups and/or multiple zones, like hisp vs others in us vs ca all on one graph\cr
#' see [Ecdf()] for options & try passing a data.frame instead of just vector\cr
#' @param scores Numeric vector (or data.frame) required. Values to analyze.
#'   If data.frame, then each column is plotted in its own panel.
#' @param pcts Numeric vector (or data.frame), required. Same number of vector elements or data.frame
#'   rows as length of scores vector (not sure what happens if pcts and scores are both data.frames).
#'   Specifies the fraction of population that is in demographic group(s) of interest, one row per place, one column per group.
#' @param pops Vector used to define weights as `pop * pcts`, and if `allothers=TRUE`, for `pop * (1-pcts)` for nongroup
#' @param weights Not used currently. See `pops` parameter
#' @param allothers Logical value, optional, TRUE by default. Whether to plot a series for everyone else, using 1-pct
#' @param col Optional, default is 'red' to signify line color red for key demographic group.
#'   Can also be a vector of colors if pcts is a data.frame with one column per group, one color per group.
#' @param main Optional character specifying plot title. Default title notes colors of lines and if reference group used.
#' @param subtitles Logical FALSE by default, which means extra info is not shown (see help on [Hmisc::Ecdf()])
#' @param ... other optional parameters to pass to Ecdf
#' @return draws a plot
#' @seealso [Hmisc::Ecdf()] [RR()] [pop.cdf()]   [pop.cdf2()] [pop.ecdf()]  [pop.cdf.density()]
#' @examples
#' ## #
#' \dontrun{
#' pop.ecdf( 31:35, c(0.10, 0.10, 0.40, 0, 0.20), 1001:1005 )
#'
#' set.seed(99)
#' pctminsim=c(runif(7000,0,1), pmin(rlnorm(5000, meanlog=log(0.30), sdlog=1.7), 4)/4)
#' popsim= runif(12000, 500, 3000)
#' esim= rlnorm(12000, log(10), log(1.15)) + rnorm(12000, 1, 0.5) * pctminsim - 1
#' pop.ecdf(esim, pctminsim, popsim,
#'  xlab='Tract air pollution levels (vertical lines are group means)',
#'   main = 'Air pollution levels among minorities (red curve) vs rest of US pop.')
#' abline(v=wtd.mean(esim, weights = pctminsim * popsim), col='red')
#' abline(v=wtd.mean(esim, weights = (1-pctminsim) * popsim), col='black')
#'
#' pop.ecdf(bg$pm, bg$pctmin, 1000,
#'  xlab='Tract air pollution levels (vertical lines are group means)',
#' main = 'PM2.5 levels among minorities (red curve) vs rest of US pop.')
#' abline(v=wtd.mean(bg$pm, weights = bg$pctmin * bg$pop), col='red')
#' abline(v=wtd.mean(bg$pm, weights = (1-bg$pctmin) * bg$pop), col='black')
#'
#' #pop.ecdf(dat$Murder, dat$Population * (dat$Illiteracy/100))
#' pop.ecdf(bg$pm, bg$pctmin, bg$pop,
#'  main='PM2.5 levels among minorities (red curve) vs rest of pop (vertical lines=group means)')
#' abline(v=wtd.mean(bg$pm, weights = bg$pctmin * bg$pop), col='red')
#' abline(v=wtd.mean(bg$pm, weights = (1-bg$pctmin) * bg$pop), col='black')
#'
#' pop.ecdf(log10(places$traffic.score), places$pctmin, places$pop)
#' pop.ecdf(places$cancer, places$pctmin, places$pop, allothers=FALSE)
#' pop.ecdf(places$cancer, places$pctlingiso, places$pop, col='green', allothers=FALSE, add=TRUE)
#' # Demog suscept  for each REGION (can't see if use vs others)
#' pop.ecdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          group=bg$REGION, allothers=FALSE,
#'          xlab='Traffic score (log scale)', ylab='%ile of population',
#'           main='Distribution of scores by EPA Region')
#'
#' # Demog suscept (how to show vs others??), one panel per ENVT FACTOR (ie per col in scores df)
#' data('names.e')
#' pop.ecdf(bg[ , names.e], bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          allothers=TRUE, ylab='%ile of population',
#'           main='Distribution of scores by EPA Region')
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
pop.ecdf <- function(scores, pcts, pops, allothers=TRUE, col='red', main='', weights, subtitles=FALSE, ...) {

	# should add error checking here

  if (missing(main)) {main <- paste('ECDF of scores in selected group (', col, ') ', ifelse(allothers, 'vs. rest of the population',''), sep='')}
  if (missing(pops)) {pops <- 1}
  if (!missing(weights)) {warning('weights parameter is currently ignored, since pops*(1-pcts) is used as weights now')}
  #require(Hmisc)

  if (is.vector(pcts)) {
    Hmisc::Ecdf(scores, weights=(pcts * pops), main=main, col=col, subtitles=subtitles, ...)
    if (allothers) { Hmisc::Ecdf(scores, weights=((1 - pcts) * pops), add=TRUE, col='black', subtitles=FALSE, ...) }
    # note can't draw subtitles (n, m) once for ref group and once for nonref group in same spot - the text would be overwritten and look wrong
  }

  if (is.data.frame(pcts)) {
    # plot one cdf per col of pcts (one per group)
    # can't just pass df of pcts to Ecdf
    # can't use groups param in Ecdf, since that assumes each group is a unique set of rows, but we need different weighting for each group to be estimated
    if (length(col)!=length(pcts[1, ])) {col=rep(col,length(pcts[1,]))}

    Hmisc::Ecdf(scores, weights=(pcts[,1] * pops), main=main, col=col[1], subtitles=subtitles, ...)
    for (i in 2:length(pcts[1, ])) {
      Hmisc::Ecdf(scores, weights=(pcts[i, ] * pops), add=TRUE, col=col[i], ...)
    }
  }
}
