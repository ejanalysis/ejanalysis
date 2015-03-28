#
# COMPARISON OF data.table vs Hmisc summarize() for weighted means of subsets of fields
#
# NOTES/ WORK IN PROGRESS

# see also  wtd.colMeans etc.

################################
# using data.table
################################
if (1==0) {
  require(data.table)
  mydata = data.table(bg, key='ST')
  
  x= mydata[, list(
    pctlowinc = sum(pctlowinc * pop) / sum(pop), 
    pctmin    = sum(pctmin    * pop) / sum(pop)
  ), 
  by = "REGION"
  ]
}

################################
# using rollup() which uses summarize() from Hmisc
################################

if (1==0) {
  require(Hmisc)
  source('rollup.R')
  
  y= rollup(bg[,c('pctlowinc', 'pctmin')], by= bg$REGION, wts=bg$pop)
  
  #There were 12 warnings (use warnings() to see them)
}
