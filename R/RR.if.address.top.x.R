#' @title Analyze how much RR would change if top-ranked places were addressed - ** NOT WORKING/ IN PROGRESS
#'
#' @description Function to analyze data on demographic and environmental (e) indicators by place (e.g., Census block group)
#'   to get stats on what percent of population or places could account for
#'   all risk ratio (RR, or ratio of mean e in one demog group vs others), and what is risk ratio if you reduce e (environmental indicator)
#'   by using some multiplier on e, in top x percent of places
#'
#' @details
#'   The effects of one place on overall RR is related to an ej.index, which here is a metric describing one place's contribution to an overall metric of disparity.
#'   RR is one overall metric of disparity, the ratio of mean environmental indicator value in one demographic group over the mean in the reference group.
#'   If RR = E/e, where E=avg environmental indicator or risk in key demographic group and e= in reference group,
#'   another metric of disparity is the excess individual risk, or E-e.
#'   The excess population risk or excess cases would be (E-e) * p * d where p=total population and d=fraction that is in key demographic group.
#'   Various counterfactuals could be used here for
#'   scenario defining what it means to address the top places and
#'   what is used to define top places.
#'
#' @param rank.by.df Data.frame of indicators to rank places by, when defining top places
#' @param e.df Environmental indicators data.frame, one row per place, required.
#' @param d.pct Demographic percentage, as fraction, defining what fraction of population in each place (row) is in demographic group of interest. Required.
#' @param popcounts Numeric vector of counts of total population in each place
#' @param d.pct.us xxxxx
#' @param or.tied Logical value, optional, TRUE by default, in which case ties of ranking variable with a threshold value (value >= threshold) are included in places within that bin.
#' @param if.multiply.e.by Optional, 0 by default. Specifies the number that environmental indicator values would be multiplied by
#'   in the scenario where some places are addressed. Zero means those top-ranked places would have the environmental indicator set to zero,
#'   while 0.9 would mean and 10 percent cut in the environmental indicator value.
#' @param zones Subsets of places such as States
#' @param mycuts optional vector of threshold values to analyze. Default is c(50,80,90:100)
#' @param silent optional logical, default is TRUE, while FALSE means more information is printed
#' @return Returns a list of results:
#'
#'   1. rrs data.frame, one column per environmental indicator, one row per threshold value
#'   2. rrs2 data.frame, Relative risks 2
#'   3. state.tables A list
#'   4. worst.as.pct Worst as percent, vector as long as number of environmental indicators
#'   5. worst.as.pct.of.bgs Worst as percent of places (e.g., block groups)
#'
#' @seealso [RR()] and [RR.if.address.top.x()] and [ej.indexes()] and [ej.added()]
#' @examples
#'  ## #
#' @export
RR.if.address.top.x <- function(rank.by.df, e.df, d.pct, popcounts, d.pct.us, or.tied=TRUE, if.multiply.e.by=0, zones=NULL, mycuts=c(50,80,90:100), silent=TRUE) {

  if (missing(d.pct.us)) {
    warning('assumed that d.pct.us is popcounts-wtd mean of d.pct, which is not right if correct denominator for d.pct is not popcounts')
    d.pct.us <- stats::weighted.mean(d.pct, w=popcounts) # *** na.rm should be explicit and differs between Hmisc::wtd.mean and stats::weighted.mean
  }

  rrs <- data.frame(matrix(NA, ncol=length(names(e.df)), nrow=length(mycuts)))
  rownames(rrs)=mycuts
  names(rrs)=names(e.df)
  worst.as.pct <- vector(length=length( names(e.df) ))
  worst.as.pct.of.bgs <- worst.as.pct
  state.tables <- list()

  for (i in 1:(length(e.df))) {

    if (!silent) { cat('\nStats for', names(e.df)[i], '-----------------------------------\n\n') }

    for (mycut.i in 1:length(mycuts) ) {

      if (or.tied) {
        worst.x <- ( rank.by.df[ , i] >= mycuts[mycut.i]) # those above given EJ %ile threshold, or specified threshold
      } else {
        worst.x <- ( rank.by.df[ , i] > mycuts[mycut.i]) # those above given EJ %ile threshold, or specified threshold
      }
      worst.x[is.na(worst.x)] <- FALSE

      new.E <- e.df[ , i]
      new.E[is.na(new.E)] <- 0  # have to set these to zero for logical subsetting to work
      new.E[worst.x] <- new.E[worst.x] * if.multiply.e.by  # THIS SETS ENVT INDICATOR SCORE TO ZERO or cuts it using specified multiplier WHERE BG IDENTIFIED AS "WORST" AS RANKED BY EJ INDEX or specified rank.by.df
      rrs[mycut.i , i] <- RR(new.E, d.pct, popcounts )
      if (!silent) {
        cat(round(  rrs[mycut.i , i], 3) )
        cat(' = RR nationwide if top', 100-mycuts[mycut.i],'% of pop (if popwtd pctiles used to rank) had', names(e.df)[i],' = ', if.multiply.e.by, " x ", names(e.df)[i])
        cat('[', round(  RR( e.df[ , i][worst.x],  d.pct[worst.x], popcounts[worst.x] ), 2) )
        cat(' = RR among just those top places ]\n')
      }
    }
    if (!silent) {
      cat(round( RR( e.df[ , i],  d.pct, popcounts ) ,3))
      cat(' = overall USA RR for ', names(e.df)[i], '\n')
      cat('\n')
    }

    # Now for a calculated threshold, not the specified threshold:
    # Calculate the threshold that will get RR==1, using specified rank.by.df and if.multiply.e.by, etc.:

    # DOES NOT WORK YET ***

    raw.ej <- popcounts * e.df[ , i] * (d.pct - d.pct.us)
    raw.ej[is.na(raw.ej)] <- 0
    raw.ej <- data.frame(raw.ej, originalrow=1:(length(raw.ej)))   # somewhere around here is super slow !!!  # somewhere around here is super slow !!!
    raw.ej <- raw.ej[ order(rank.by.df[ , i]) , ]  # somewhere around here is super slow !!!
    cum.raw.ej <- cumsum(raw.ej[,1])
    cum.raw.ej <- cum.raw.ej[order(raw.ej$originalrow)]  # somewhere around here is super slow !!!
    worst.x <- (cum.raw.ej < 0) # check if < or > here
    worst.x[is.na(worst.x)] <- FALSE

    worst.as.pct.of.bgs[i] <- mean(worst.x)
    worst.as.pct[i] <- sum( popcounts[worst.x] ) / sum(popcounts)
    new.E <- e.df[ , i]
    new.E[is.na(new.E)] <- 0
    new.E[worst.x] <- (new.E[worst.x]) * if.multiply.e.by  # THIS SETS ENVT INDICATOR SCORE TO ZERO (or whatever specified) WHERE BG IDENTIFIED AS "WORST" AS RANKED BY EJ INDEX (or specified)
    if (!silent) {
      cat(round(  RR( new.E, d.pct, popcounts ) , 3))
      cat(' = RR nationwide if top', round(100 * worst.as.pct[i], 2),'% of people (if popwtd pctiles used to rank) in top', round(100 * worst.as.pct.of.bgs[i], 2),'% of places had E=E x',if.multiply.e.by,'\n')
      if (!is.null(zones)) {
        cat('Locations of those places:\n')
        print( state.tables[[i]] <- sort(table(zones[worst.x]), decreasing=TRUE) )
      } else {state.tables[[i]] <- 'no zones specified'}
    }
  }

  rrs2=rbind(round(rrs,3), key.pctile=paste(round(100 - 100 * worst.as.pct, 3),'%',sep=''), key.pct=paste(round(100 * worst.as.pct,3),'%',sep=''))
  names(worst.as.pct) <- names(e.df)
  names(worst.as.pct.of.bgs) <- names(e.df)

  return(list(
    rrs=rrs,
    rrs2=rrs2,
    state.tables=state.tables,
    worst.as.pct=worst.as.pct,
    worst.as.pct.of.bgs=worst.as.pct.of.bgs)
    )

  # EXAMPLE
  #
  # VSI.eo.US <- with(bg, ( sum(mins) / sum(pop) + sum(lowinc) / sum(povknownratio) ) / 2)
    ## or for 2008-2012 use...
  # VSI.eo.US <- 0.3493374
  #
  # results <- RR.if.address.top.x(rank.by.df=bg[,names.ej.pctile], e.df, d.pct, popcounts, d.pct.us=VSI.eo.US,
  #    zones=bg$ST, or.tied=TRUE, if.multiply.e.by=0, mycuts=c(50,80,90:100), silent=TRUE)
  # #results$rrs2
  #
  # str(results)
#
#   List of 5

#   $ rrs                :'data.frame':  13 obs. of  12 variables:
#     ..$ pm             : num [1:13] 0.685 0.885 0.958 0.964 0.97 ...
#   ..$ o3             : num [1:13] 0.731 0.902 0.949 0.954 0.958 ...
#   ..$ cancer         : num [1:13] 0.59 0.877 1.011 1.025 1.038 ...
#   ..$ neuro          : num [1:13] 0.576 0.831 0.979 0.997 1.015 ...
#   ..$ resp           : num [1:13] 0.565 0.883 1.044 1.062 1.08 ...
#   ..$ dpm            : num [1:13] 0.524 0.856 1.09 1.119 1.149 ...
#   ..$ pctpre1960     : num [1:13] 0.511 0.803 1.001 1.022 1.044 ...
#   ..$ traffic.score  : num [1:13] 0.541 0.799 1.006 1.036 1.066 ...
#   ..$ proximity.npl  : num [1:13] 0.512 0.734 0.878 0.897 0.914 ...
#   ..$ proximity.rmp  : num [1:13] 0.551 0.793 0.984 1.011 1.039 ...
#   ..$ proximity.tsdf : num [1:13] 0.526 0.769 0.965 0.991 1.019 ...
#   ..$ proximity.npdes: num [1:13] 0.55 0.766 0.881 0.896 0.914 ...

#   $ rrs2               :'data.frame':	15 obs. of  12 variables:
#     ..$ pm             : chr [1:15] "0.685" "0.885" "0.958" "0.964" ...
#   ..$ o3             : chr [1:15] "0.731" "0.902" "0.949" "0.954" ...
#   ..$ cancer         : chr [1:15] "0.59" "0.877" "1.011" "1.025" ...
#   ..$ neuro          : chr [1:15] "0.576" "0.831" "0.979" "0.997" ...
#   ..$ resp           : chr [1:15] "0.565" "0.883" "1.044" "1.062" ...
#   ..$ dpm            : chr [1:15] "0.524" "0.856" "1.09" "1.119" ...
#   ..$ pctpre1960     : chr [1:15] "0.511" "0.803" "1.001" "1.022" ...
#   ..$ traffic.score  : chr [1:15] "0.541" "0.799" "1.006" "1.036" ...
#   ..$ proximity.npl  : chr [1:15] "0.512" "0.734" "0.878" "0.897" ...
#   ..$ proximity.rmp  : chr [1:15] "0.551" "0.793" "0.984" "1.011" ...
#   ..$ proximity.tsdf : chr [1:15] "0.526" "0.769" "0.965" "0.991" ...
#   ..$ proximity.npdes: chr [1:15] "0.55" "0.766" "0.881" "0.896" ...

#   $ state.tables       : list()

#   $ worst.as.pct       : Named num [1:12] 0.991 0.993 0.951 0.962 0.938 ...
#   ..- attr(*, "names")= chr [1:12] "pm" "o3" "cancer" "neuro" ...

#   $ worst.as.pct.of.bgs: Named num [1:12] 0.988 0.989 0.959 0.964 0.946 ...
#   ..- attr(*, "names")= chr [1:12] "pm" "o3" "cancer" "neuro" ...

}
