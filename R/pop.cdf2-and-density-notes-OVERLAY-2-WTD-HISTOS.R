# if (1==0) {
#   # HOW TO PLOT WEIGHTED HISTOGRAMS, OVERLAID 1 DEMOG GROUP VS ALL OTHERS:
#
#   # ALSO SEE  https://plot.ly/ggplot2/geom_histogram/
#
#   # see pop.cdf2
#   # see pop.cdf
#   # see pop.cdf.density.R
#   # see pop.ecdf
#
#
#   # OVERLAY OF 2 histograms
#
#   require(ejanalysis)
#   # ?"ejanalysis-package"
#
#   pop.ecdf(bg$cancer, bg$pctmin, bg$pop, allothers=FALSE); pop.ecdf(bg$cancer, bg$pctlingiso, bg$pop, col='green', allothers=FALSE, add=TRUE)
#
#   # Demog suscept  for each REGION (can't see if use vs others)
#   pop.ecdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
#            group=bg$REGION, allothers=FALSE,
#            xlab='Traffic score (log scale)', ylab='%ile of population', main='Distribution of scores by EPA Region')
#
#   pop.ecdf(bg$pm, bg$pctmin, 1000, xlab='Tract air pollution levels (vertical lines are group means)',
#            +          main = 'PM2.5 levels among minorities (red curve) vs rest of US pop.')
#   abline(v=wtd.mean(bg$pm, weights = bg$pctmin * bg$pop), col='red')
#   abline(v=wtd.mean(bg$pm, weights = (1-bg$pctmin) * bg$pop), col='black')
#   #?plot
#   axis(side = 1, at = 4:14 )
# }
