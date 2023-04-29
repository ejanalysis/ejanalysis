#' @title Draw PDF (overlays histograms) comparing distributions of scores in selected demographic groups
#'
#' @description
#'  Draws a histogram plot using [plotrix::weighted.hist()], overlaying distribution functions,
#'  one for each subgroup specified.
#'  Useful to compare 2 groups based on each groups entire pdf distribution of peoples scores,
#'  using data from small places like census block groups, based on having for each place the pop total
#'  and % of pop that is in each group or perhaps already have count in each group.
#' @details
#' Notes: \cr
#' to compare zones, \cr
#' compare demog groups, (see parameter called group)\cr
#' compare multiple groups and/or multiple zones, like hisp vs others in us vs ca all on one graph\cr
#' see [plotrix::weighted.hist()] for options \cr
#' @param scores Numeric vector (not data.frame currently), required. Values to analyze.
#' @param pcts Numeric vector or data.frame, required. Same number of vector elements or data.frame
#'   rows as length of scores. Specifies the fraction of population that is in demographic group(s) of interest, one row per place, one group per column.
#' @param pops Vector used to define weights as pop*pcts, and if allothers=TRUE, for pop*(1-pcts) for nongroup
#' @param weights Not used currently (see `pop` parameter)
#' @param allothers Logical value, optional, TRUE by default. Whether to plot a series for everyone else, using 1-pct
#' @param col Optional, default is 'red' to signify line color red for key demographic group.
#'   Can also be a vector of colors if pcts is a data.frame with one column per group, one color per group.
#' @param main Optional character specifying plot title. Default title notes colors of lines and if reference group used.
#' @param ... other optional parameters to pass to weighted.hist()
#' @return Draws a plot
#' @seealso [Hmisc::Ecdf()] [RR()] [pop.cdf()]   [pop.cdf2()] [pop.ecdf()]  [pop.cdf.density()]
#' @examples
#' ## #
#' \dontrun{
#'
#'
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
#'
#' # pop.cdf( 31:35, c(0.10, 0.10, 0.40, 0, 0.20), 1001:1005 )
#'
#' set.seed(99)
#' pctminsim=c(runif(7000,0,1), pmin(rlnorm(5000, meanlog=log(0.30), sdlog=1.7), 4)/4)
#' popsim= runif(12000, 500, 3000)
#' esim= rlnorm(12000, log(10), log(1.15)) + rnorm(12000, 1, 0.5) * pctminsim - 1
#' pop.cdf(esim, pctminsim, popsim, xlab='Tract air pollution levels',
#'   main = 'Air pollution levels among minorities (red bars) vs rest of US pop.')
# x1 <- weighted.mean(esim, weights = pctminsim * popsim)
# x2 <- weighted.mean(esim, weights = (1-pctminsim) * popsim)
#'
#  pop.cdf(dat$Murder, dat$Population * (dat$Illiteracy/100))
#' #
#' # pop.cdf(bg$pm, bg$pctmin, bg$pop)
#' # pop.cdf(log10(places$traffic.score), places$pctmin, places$pop)
#' # pop.cdf(places$cancer, places$pctmin, places$pop, allothers=FALSE)
#' # pop.cdf(places$cancer, places$pctlingiso, places$pop, col='green', allothers=FALSE, add=TRUE)
#' # Demog suscept  for each REGION (can't see if use vs others)
#' pop.cdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          group=bg$REGION, allothers=FALSE,
#'          xlab='Traffic score (log scale)', ylab='frequency in population',
#'           main='Distribution of scores by EPA Region')
#'
#' # Demog suscept (how to show vs others??), one panel per ENVT FACTOR (ie per col in scores df)
#' data('names.e')
#' # NOT
#' pop.cdf(bg[ , names.e], bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#'          allothers=TRUE, ylab='frequency in population',
#'           main='Distribution of scores by EPA Region')
#'
#' # log scale is useful & so are these labels passed to function
#' # in CA vs not CA
#' pop.cdf(bg$traffic.score, bg$ST=='CA', bg$pop,
#'          subtitles=FALSE,
#'          log='x', ylab='frequency in population', xlab='Traffic scores (log scale)',
#'          main='Distribution of scores in CA (red) vs rest of US')
#'
#' # Flagged vs not (all D, all zones)
#' pop.cdf(bg$traffic.score, bg$flagged, bg$pop, log='x')
#'
#' # D=Hispanics vs others, within CA zone only
#' pop.cdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$pcthisp, log='x')
#' # Demog suscept vs others, within CA only
#' pop.cdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$VSI.eo, log='x')
#'
#' }
#' @export
pop.cdf <- function(scores, pcts, pops, allothers=TRUE, col='lightblue', main, weights, ...) {
  # e, dcount, refcount, etxt, dtxt, brks=10, ...

  if (missing(main)) {main <- paste0(
    'Histogram of scores in selected group (', col, ') ',
    ifelse(allothers, 'vs. rest of the population', '')
  )}
  if (missing(pops)) {pops <- 1}
  if (!missing(weights)) {warning('weights parameter is currently ignored, since pops*(1-pcts) is used as weights now')}

  if (length(dim(scores))!=0 & length(dim(pcts))!=0) {
    stop('Scores or pcts must be a vector -- They cannot both be data.frames or have 2+ dimensions currently')
    # could just say is.vector(scores) & ... but a list() is also a vector.
  }

  valids <- !is.na(scores)
  if (length(dim(scores))!=0) {
    # 2+ dimensions so !is.na creates an array index called valids

    # This would skip the pop, pct, and scores for any place where ANY ONE of the scores is invalid:
    # pops <- pops[rowMins(valids)]
    # pcts <- pcts[rowMins(valids)]
    # scores <- scores[rowMins(valids)]

  } else {
    # scores is just a vector, so valids is too
    scores <- scores[valids]
    pops   <- pops[valids]
    if (length(dim(pcts))==0) {
      #pcts is just a vector
      pcts <- pcts[valids]
    } else {
      # scores is a vector but pcts is not (multiple groups for one score type)
      pcts <- pcts[valids, ] # get rid of all groups in row (place) where scores invalid
    }
  }

  # still need to handle case where multiple scores, one group

  if (allothers) {
    plotrix::weighted.hist(x=scores, w=      pcts  * pops, freq=FALSE, col=col,  main=main, xlim=range(scores), ...)
    plotrix::weighted.hist(x=scores, w= (1 - pcts) * pops, freq=FALSE, add=TRUE, main=main, ...)
  }

  if (length(dim(pcts))==2) {
    # plot one histo per col of df
    # can't pass df to weighted.hist
    if (length(col)!=length(pcts[1, ])) {col=rep(col,length(pcts[1,]))}

    plotrix::weighted.hist(scores,       pcts[ , 1] * pops, freq=FALSE, col=col[1], main=main, ...)
    for (i in 2:length(pcts[1,])) {
      plotrix::weighted.hist(scores,     pcts[ , i] * pops, freq=FALSE, col=col[i], add=TRUE, ...)
    }
  }

  legend(x = "right", legend = c("Reference", dtxt), fill = c("white", "lightblue"))

  #plotrix::weighted.hist(bg$proximity.rmp, bg$pop * bg$pctmin,     xlim=c(0,1.8), freq=FALSE, breaks=c(0:18)/10 , col='red', main='pop hist for pctmin of RMP score')
  #plotrix::weighted.hist(bg$proximity.rmp, bg$pop * (1-bg$pctmin), xlim=c(0,1.8), freq=FALSE, breaks=c(0:18)/10 , add=TRUE)
}
