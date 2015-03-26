

pop.ecdf <- function(scores, pcts, pops, allothers=TRUE, col='red', main, weights, subtitles=FALSE, ...) {

	# should add error checking here
  
  if (missing(main)) {main <- paste('ECDF of scores in selected group (', col, ') ', ifelse(allothers, 'vs. rest of the population',''), sep='')}
  if (missing(pops)) {pops <- 1}
  if (!missing(weights)) {warning('weights parameter is currently ignored, since pops*(1-pcts) is used as weights now')}
  require(Hmisc)  
  
  if (is.vector(pct)) {
    Ecdf(scores, weights=(pcts * pops), main=main, col=col, subtitles=subtitles, ...)    
    if (allothers) { Ecdf(scores, weights=((1 - pcts) * pops), add=TRUE, col='black', subtitles=FALSE, ...) }
    # note can't draw subtitles (n, m) once for ref group and once for nonref group in same spot - the text would be overwritten and look wrong
  }

  if (is.data.frame(pct)) {
    # plot one cdf per col of df
    # can't pass df to Ecdf because that tries to draw one cdf per panel, multiple panels
    # can't use groups param in Ecdf, since that assumes each group is a unique set of rows, but we need different weighting for each group to be estimated
    for (i in 1:length())
    Ecdf(scores, weights=(pcts * pops), main=main, col=col, subtitles=subtitles, ...)        
  }

  if (1==0) {
  ###############
  # FUNCTIONS TO COMPARE 2 GROUPS BASED ON EACH GROUP'S ENTIRE PDF OR CDF / DISTRIBUTION OF PEOPLE'S SCORES, 
  # USING DATA FROM SMALL PLACES LIKE CENSUS BLOCK GROUPS, BASED ON HAVING FOR EACH PLACE THE POP TOTAL AND % OF POP THAT IS IN EACH GROUP
  # OR PERHAPS ALREADY HAVE COUNT IN EACH GROUP.
  # for wtd.Ecdf() see
  # http://127.0.0.1:11612/library/Hmisc/html/wtd.stats.html
  #
  # usage:
  #
  # pop.ecdf( 31:35, c(0.10, 0.10, 0.40, 0, 0.20), 1001:1005 )
  # pop.ecdf(dat$Murder, dat$Population * (dat$Illiteracy/100))
  #
  # pop.ecdf(places$pm, places$pctmin, places$pop)
	# pop.ecdf(log10(places$traffic.score), places$pctmin, places$pop)
	# pop.ecdf(places$cancer, places$pctmin, places$pop, allothers=FALSE); pop.ecdf(places$cancer, places$pctlingiso, places$pop, col='green', allothers=FALSE, add=TRUE)

  # NOTES ON USING pop.ecdf()
  #
  # to compare zones,
  # compare demog groups, (see parameter called group)
  # compare multiple groups and/or multiple zones, like hisp vs others in us vs ca all on one graph
  # see ?Ecdf for options & try passing a data.frame instead of just vector
  
  # Demog suscept  for each REGION (can't see if use vs others)
  pop.ecdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
           group=bg$REGION, allothers=FALSE,
           xlab='Traffic score (log scale)', ylab='%ile of population', main='Distribution of scores by EPA Region')
  
  # Demog suscept (how to show vs others??), one panel per REGION
  pop.ecdf(bg[ , names.e], bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
          allothers=TRUE, ylab='%ile of population', main='Distribution of scores by EPA Region')
  
  # log scale is useful & so are these labels passed to function
  # in CA vs not CA
  pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop, 
           subtitles=FALSE,
           log='x', xlab='%ile of population', ylab='Traffic scores (log scale)',
           main='Distribution of scores in CA (red) vs rest of US')
    
  # Flagged vs not (all D, all zones)
  pop.ecdf(bg$traffic.score, bg$flagged, bg$pop, log='x')
  
  # D=Hispanics vs others, within CA zone only
  pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$pcthisp, log='x')
  # Demog suscept vs others, within CA only
  pop.ecdf(bg$traffic.score, bg$ST=='CA', bg$pop * bg$VSI.eo, log='x')

  
  }
}
