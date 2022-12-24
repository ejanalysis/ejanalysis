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
  require(ejanalysis)
  #  # ?"ejanalysis-package"
  bg <- ejscreen::bg22[, c(ejscreen::names.d, 'pop', ejscreen::names.e, 'REGION')]

  e <- bg$pm[!is.na(bg$pm)]
  dpct <- bg$pctmin
  dcount   <- bg$pop[!is.na(bg$pm)] *      dpct[!is.na(bg$pm)]
  refcount <- bg$pop[!is.na(bg$pm)] * (1 - dpct[!is.na(bg$pm)])
  brks <- 0:17
  etxt <- 'PM2.5'
  dtxt <- 'Minorities'

  pop.cdf(        e, pcts = dpct, pops = bg$pop)
  pop.cdf2(       e, dcount, refcount, etxt, dtxt, brks)
  pop.cdf.density(e, dcount, refcount, etxt, dtxt )



   # e = log10(bg$proximity.rmp); e[is.infinite(e)] <- NA

  pop.ecdf(e,   bg$pctmin,    bg$pop, col='red',   allothers=FALSE, main = 'RMP proximity scores within each group')
  pop.ecdf(e,   bg$pctlowinc, bg$pop, col='green', allothers=FALSE, add=TRUE)
  pop.ecdf(e, 1-bg$pctmin,    bg$pop, col='black', allothers=FALSE, add=TRUE)
  pop.ecdf(e, 1-bg$pctlowinc, bg$pop, col='gray',  allothers=FALSE, add=TRUE)
  legend(x = 'bottomright',
         legend = c('Non-POC', 'Non-Low-Income', 'POC', 'Low income' ),
         fill   = c('black',    'gray',           'red', 'green'))

  pop.cdf.density( e =   log10(bg$proximity.tsdf), dcount =  bg$pop * bg[, c( "pctmin")],  refcount = bg$pop * (1 - bg$pctmin), etxt = 'TSDF', dtxt = 'People of Color')

  pop.cdf(bg$proximity.tsdf, bg$pctmin, bg$pop, main = "Histogram of TSDF scores in POC and non-POC")



  # Demog suscept  for each REGION (can't see if use vs others)
  pop.ecdf(bg$traffic.score, bg$VSI.eo, bg$pop, log='x', subtitles=FALSE,
           group=bg$REGION, allothers=FALSE,
           xlab='Traffic score (log scale)', ylab='%ile of population',
           main='Distribution of scores by EPA Region')

  pop.ecdf(bg$pm, bg$pctmin, 1000, xlab='Tract air pollution levels (vertical lines are group means)',
                      main = 'PM2.5 levels among minorities (red curve) vs rest of US pop.')
  abline(v=wtd.mean(bg$pm, weights = bg$pctmin * bg$pop), col='red')
  abline(v=wtd.mean(bg$pm, weights = (1-bg$pctmin) * bg$pop), col='black')
  #?plot
  axis(side = 1, at = 4:14 )
# }
