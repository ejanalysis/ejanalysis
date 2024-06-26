#' @title Assign percentiles to vector of values (weighted, by zone)
#'
#' @description For the vector, look at the distribution of values across all rows in a given zone (e.g., places in zone),
#' and find what percentile a given value is at.
#' @details Relies on the [Hmisc::wtd.Ecdf()] function.
#'   COULD BE RECODED TO BE FASTER USING data.table package *** see notes in [rollup.pct()] and [rollup()]
#'   Could also add parameter like in rank(), na.last,
#'   defining na.rm but also where to rank NA values if included, etc.
#'
#'   Default now is like na.last=NA, but like na.last='last' if na.rm=FALSE
#'
#'   ****Could also add parameter like in rank(), ties.method, defining if ties get min, max, or mean of percentiles initially assigned to ties.
#'   Default for ties up to mid2022 was like ties.method=max ?
#'   But EJScreen will redefine percentiles to set tied values at lower end of that group, min
#'
#' @param values vector, required, with numeric values. To do this with a matrix, see [make.pctile.cols()]
#' @param weights Optional, NULL by default (not fully tested), vector of weights for weighted percentiles (e.g., population weighted).
#' @param na.rm NOT IMPLEMENTED HERE. Logical, optional, TRUE by default. Should NA values (missing data) be removed first to get percentile of those with valid data.
#'   If FALSE, NA values are treated as being at the high percentiles.
#' @param zone Optional, NULL by default, defines subsets of rows, so a percentile is found among rows that are within a given zone only.
#' @return Returns a numeric vector same size as x, but if zone is specified, provides percentile with given zone.
#' @seealso
#' [make.bin.pctile.cols()] to call functions below, converting columns of values to percentiles and then bins
#' [assign.pctiles()] for one vector, assign (weighted) percentile (quantile) to each value within its zone (subset)
#' [assign.pctiles.alt2()] as an alternative method, to replicate assign.pctiles, but not by zone
#' [get.pctile()] to get (weighted) percentile of just 1+ values within given vector of values
#' [make.pctile.cols()] for a data.frame, assign percentiles, return a same-sized df that is wtd.quantile of each value within its column
#' [make.pctile.cols.alt2()] as an alternative method, to replicate make.pctile.cols
#' [assign.map.bins()] for one vector (or data.frame) of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks.
#' [make.bin.cols()] for a data.frame of values (e.g. percentiles), return same-sized df that is bin number (map color bin) using preset breaks.
#' [write.pctiles()] to save file that is lookup table of percentiles for columns of a data.frame
#' [write.pctiles.by.zone()] to save file that is lookup table of percentiles for columns of a data.frame, for each geographic zone (subset of rows)
#' [write.wtd.pctiles()] to save file that is lookup table of weighted percentiles for columns of a data.frame
#' [write.wtd.pctiles.by.zone()] to save file that is lookup table of weighted percentiles for columns of a data.frame, for each geographic zone (subset of rows)
#' [lookup.pctile()] to look up current approx weighted percentiles in a lookup table that is already in global memory
#'
#' @examples
#' x <- c(30, 40, 50, 12,12,5,5,13,13,13,13,13,8,9,9,9,9,9,10:20,20,20,20,21:30)
#' wts <- rep(c(2,3), length(x)/2)
#' cbind(wts, x, PCTILE=assign.pctiles(x,wts))
#'
#' # PERCENTILE OF ALL, NOT JUST THOSE WITH VALID DATA, IF na.rm=FALSE,
#' # but then NA values preclude high percentiles:
#' x <- c(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA,13,13,8,9,9,9,9,9,10:20,20,20,20,21:30)
#' wts <- rep(c(2,3), length(x)/2)
#' cbind(wts, x, PCTILE.alt2=assign.pctiles.alt2(x, wts, na.rm=FALSE),
#'  pctile=assign.pctiles(x,wts))[order(x),]
#' cbind(wts, x, PCTILE.alt2=assign.pctiles.alt2(x, wts, na.rm=TRUE),
#'  pctile=assign.pctiles(x,wts))[order(x),]
#'
#' V=9
#' sum(wts[!is.na(x) & x <= V]) / sum(wts[!is.na(x)])
#'
#' #A value (V) being at this PCTILE% means that (assuming na.rm=TRUE):
#'
#' # V >= x  for        PCTILE% of wts     (for non-NA x), so
#' # V < x   for 100% - PCTILE% of wts     (for non-NA x), or
#' # PCTILE% of all wts have V >= x (for non-NA x), so
#' # 100% - PCTILE% of all wts have V < x  (for non-NA x).
#'
#' x <- c(32, NA, NA, NA,NA,NA,NA,NA,NA,NA,13,13,8,9,9,9,9,9,10:20,20,NA,20,21:30)
#' wts <- rep(c(2,3), length(x)/2)
#' cbind(wts, x, PCTILE.alt2=assign.pctiles.alt2(x, wts, na.rm=FALSE),
#'  pctile=assign.pctiles(x,wts))[order(x),]
#' cbind(wts, x, PCTILE.alt2=assign.pctiles.alt2(x, wts, na.rm=TRUE),
#'  pctile=assign.pctiles(x,wts))[order(x),]
#'
#'  \dontrun{
#'    What is environmental score at given percentile?
#'  ejanalysis::lookup.pctile(40,'cancer',lookupUSA)
#'  # [1] 84
#'  ejanalysis::lookup.pctile(40,'cancer',lookupStates,'WV')
#'  # [1] 93
#'  #    What is percentile of given environmental score?
#'  ejscreen::lookupUSA[lookupUSA$PCTILE=='84' ,'cancer']
#'  # [1] 39.83055
#'  ejscreen::lookupStates[lookupStates$PCTILE=='84' & lookupStates$REGION =='WV','cancer']
#'  # [1] 33.36371
#'  }
#'
#' @export
#'
assign.pctiles <- function(values, weights=NULL, zone=NULL, na.rm=TRUE) {

if (all(weights == 1)) {weights <- NULL} # to work with make.pctile.cols( where default was weights=1 )

  #  FUNCTION TO ASSIGN THE EXACT WEIGHTED (or unweighted) PERCENTILE NUMBER TO EACH LOCATION'S RAW INDICATOR SCORE
  if (!missing(na.rm))	{warning('na.rm is not implemented yet')}
  #require(Hmisc)

  if (is.null(zone)) {

    wtd.Ecdf.results <- Hmisc::wtd.Ecdf(values, weights=weights, normwt = FALSE, type='i/n', na.rm=TRUE)

    # If the first CDF estimate is greater than zero, a point (min(x),0) is placed at the beginning of the estimates.
    if (length(wtd.Ecdf.results$x) == 1+ length(unique(values)) ) { wtd.Ecdf.results$x <- wtd.Ecdf.results$x[-1]; wtd.Ecdf.results$ecdf <- wtd.Ecdf.results$ecdf[-1] }

    # This finds the rank of each value   # but note this is to index by number, not character value which would find the first of a given value via [as.character(   $x)]
    myindex <- findInterval(values, wtd.Ecdf.results$x)
    myindex[myindex==0] <- 1	# if for some reason the minimum value doesn't match, which has occurred in testing

    exact.wtd.pctile <- wtd.Ecdf.results$ecdf[ myindex ]

  }	else {

    # ****   this calculation by zone could be redone using aggregate, summarize, data.table functions etc. instead of a for loop:

    exact.wtd.pctile <- vector(length=length(values))

    for (z in unique(zone)) {

      myvals <- values[zone==z]
      mywts <- weights[zone==z]
      if (all(is.na(myvals)) | all(is.na(mywts))) {
        # add code here for when zone has no valid weights or no valid values due to missing values

        exact.wtd.pctile[zone==z] <- NA

      } else {
        wtd.Ecdf.results <- Hmisc::wtd.Ecdf(myvals, weights=mywts, normwt = FALSE, type='i/n', na.rm=TRUE)
        # If the first CDF estimate is greater than zero, a point (min(x),0) is placed at the beginning of the estimates.

        if (length(wtd.Ecdf.results$x) == 1+ length(unique(myvals)) ) { wtd.Ecdf.results$x <- wtd.Ecdf.results$x[-1]; wtd.Ecdf.results$ecdf <- wtd.Ecdf.results$ecdf[-1] }

        # This finds the rank of each value
        myindex <- findInterval(myvals, wtd.Ecdf.results$x)
        myindex[myindex==0] <- 1	# if for some reason the minimum value doesn't match, which has occurred in testing

        exact.wtd.pctile[zone==z] <- wtd.Ecdf.results$ecdf[ myindex ]
      }
    }
  }
  return(exact.wtd.pctile)

  ############################################################## #
  #	NOTES ON THIS FUNCTION

  #	For documentation of wtd.Ecdf() from Hmisc package, see
  #	http://127.0.0.1:26624/library/Hmisc/html/wtd.stats.html
  #	wtd.Ecdf returns a list whose elements x and Ecdf correspond to unique sorted values of x.
  #
  #	Min and max percentiles assigned:
  #	Note that if the first CDF estimate is greater than zero, a point (min(x),0) is placed at the beginning of the estimates. [so we remove that since it is just confusing]
  #	There will never be exactly 0 assigned as the percentile.
  #	The smallest that may be assigned is 1/n and this is assigned to the min input if it is unique.
  #	If min is not unique, all those N ties for min of n total values are assigned N/n (not 1/n) as their percentile.
  #	There highest percentile assigned is exactly 1.00 -- This happens to the maximum value, and for any tied for the maximum.
  #
  #	TIES: Ties in input values are all assigned the same percentile, the percentile at the upper end of that bin's range
  #	so if for example the top 3 values of 100 values are tied, they are all assigned 1.000 as the percentile & the next is assigned 0.97, etc.
  #
  #	type="i/n" to use the inverse of the empirical distribution function, using, ... wt/T,
  #		where wt is the cumulative weight and T is the total weight (usually total sample size).
  #
  #	For documentation on findInterval() function:
  #	http://127.0.0.1:26624/library/base/html/findInterval.html
  #
  #	By the way, for unweighted quantiles, the following would do almost the same thing as using wtd.Ecdf(x, weights=NULL) but not exactly identical:
  #	exact.quantile <- function(x) { Fn<-ecdf(x); Fn(x) }
  #
  #	**** Note that this assign.pctiles() function, which relies on wtd.Ecdf(),
  #	will ignore NA values, and return NA as the percentile assigned to those,
  #	and the other percentiles assigned are among valid (non-NA) values only.
  #	If na.rm=TRUE (default), this version treats NA values in input by excluding them from ranking,
  #	making their percentile NA and finding %iles among all valid values,
  #	so if there were 105 values with 5 being NA and the other 100 being unique numbers,
  #	this function would return 1-100 as the valid %iles and return "NA" as %ile for other 5.
  #	For example:
  #	100 * assign.pctiles(c(1001:1100, rep(NA, 39)), na.rm=FALSE)
  #
  #	returns the following:
  #
  #1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023
  #   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23
  #1024 1025 1026 1027 1028 1029 1030 1031 1032 1033 1034 1035 1036 1037 1038 1039 1040 1041 1042 1043 1044 1045 1046
  #  24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46
  #1047 1048 1049 1050 1051 1052 1053 1054 1055 1056 1057 1058 1059 1060 1061 1062 1063 1064 1065 1066 1067 1068 1069
  #  47   48   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69
  #1070 1071 1072 1073 1074 1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087 1088 1089 1090 1091 1092
  #  70   71   72   73   74   75   76   77   78   79   80   81   82   83   84   85   86   87   88   89   90   91   92
  #1093 1094 1095 1096 1097 1098 1099 1100 <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
  #  93   94   95   96   97   98   99  100   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
  #<NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
  #  NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
  #<NA>
  #  NA
  #
}
