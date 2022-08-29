#' @title create lookup table as file of pop-weighted or unwtd percentiles, mean, sd for US or by state or region
#' @description check which functions actually get used for this now.
#' @details
#'   also see \cr\cr
#'   ejscreen.lookuptables ??   \cr\cr
#'
#'   make.bin.pctile.cols   uses  assign.pctiles
#'
#'   write.wtd.pctiles.by.zone  uses wtd.pctiles.exact or pctiles.exact  \cr\cr
#'     pctiles.exact uses                  quantile(x, type = 1, probs = (1:100)/100, na.rm = TRUE))
#'       The inverse of quantile is ecdf  (empirical cumulative distribution function)
#'        a step function with jumps i/n at observation values, where i is the number of tied observations at that value. Missing values are ignored.
#'
#'  For observations x= (x1,x2, ... xn), Fn is the fraction of observations less or equal to t, i.e.,
#'
#'  Fn(t) = #{xi <= t}/n = 1/n sum(i=1,n) Indicator(xi <= t).
#'
#'       quantile() can use nine different quantile algorithms discussed in Hyndman and Fan (1996), -- Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical packages, American Statistician 50, 361–365. doi: 10.2307/2684934.
#'       defined as weighted averages of consecutive order statistics.
#'       type 1 is used here, and is "Inverse of empirical distribution function."
#'
#'       Sample quantiles of type i are defined by:
#'
#'                    Q[i](p) = (1 - γ) x[j] + γ x[j+1],
#'
#'                    i = type of formula to use (1 through 9).
#'                    p is the percentage (0 through 1).
#'                    x[j] is the jth order statistic,
#'                    (j-m)/n ≤ p < (j-m+1)/n,
#'                    n is the sample size, the value of
#'                    γ is a function of
#'                      j = floor(np + m)     --so this is roughly how many data points are smaller.
#'                      g =       np + m - j,  --so this is roughly
#'                      m = a constant determined by the sample quantile type. (m= 0 or -0.5 here)
#'         Discontinuous sample quantile types 1, 2, and 3
#'
#'         For types 1, 2 and 3, Q[i](p) is a discontinuous function of p, with
#'         m = 0 when i = 1 or i = 2, and m = -1/2 when i = 3.
#'         Type 1 =  Inverse of empirical distribution function. γ = 0 if g = 0, and 1 otherwise.
#'
#'     wtd.pctiles.exact uses   Hmisc::wtd.quantile(x, wts, type = "i/n", probs = (1:100)/100) ,  na.rm = na.rm))
#'       "i/n" uses the inverse of the empirical distribution function,
#'       using  wt/T, where wt is the cumulative weight and T is the total weight (usually total sample size).
#'
#'   table.pop.pctile  and \cr\cr
#'   map service with lookup tables \cr\cr \cr\cr
#' \preformatted{
#' }
#'
#' @param mydf data.frame with numeric data. Each column will be examined to calculate mean, sd, and percentiles, for each zone.
#' @param wts optional vector of numbers such as population counts as weights, as long as nrow(mydf)
#' @param filename prefix to use for filename to be saved locally (.csv is added by the function). If not provided, no file is saved.
#' @param zone.vector optional names of states or regions, for example. same length as wts, or rows in mydf
#' @param zoneOverallName optonal If not by zone, name of entire domain to use in table column called REGION. Default is USA.
#' @examples
#'   \dontrun{
#'   bg = ejscreen::bg21plus # want demog subgroups but also want PR eventually
#'   pctilevariables <- c(names.e, names.d, names.d.subgroups, names.ej)
#'    ejanalysis::write.wtd.pctiles(mydf = bg[ , pctilevariables], wts = bg$pop, filename =  'lookupUSA21')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$REGION, filename =  'lookupRegions21')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$ST,     filename =  'lookupStates21')
#'   }
#'
#' @export
write.wtd.pctiles.by.zone <- function(mydf, wts=NULL, filename=NULL, zone.vector=NULL, zoneOverallName='USA') {

  # FUNCTIONS TO SHOW percentile STATS STRATIFIED BY REGION (OR STATE),
  #	SAVING A FILE OF STATS FOR EACH REGION OR STATE
  #  LIKE A SET OF LOOKUP TABLES

  # but now will fix it to just create a single table or file for the whole group of zones like all States in 1 csv file, as EJScreen would use.

  if (is.null(zone.vector)) {
    if (is.null(wts)) {

      # NO WEIGHTS NO ZONES ####

      r <- data.frame(sapply(mydf, function(x) pctiles.exact(x)))
      r <- rbind(r, t(data.frame(mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
      r <- rbind(r, t(data.frame(std.dev = sapply(mydf, function(x) sd(x, na.rm = TRUE)))))
      r$REGION <- zoneOverallName
      r$PCTILE <- rownames(r) # 1:100,'mean','std.dev'
    } else {

      # WEIGHTS, BUT NO ZONES ####

      r = data.frame(sapply(mydf, function(x) analyze.stuff::wtd.pctiles.exact(x, wts) ) )
      r = rbind(r, t(data.frame(mean=sapply(mydf, function(x) Hmisc::wtd.mean(x, wts, na.rm=TRUE) ) ) ))
      r = rbind(r, t(data.frame(std.dev=sapply(mydf, function(x) sqrt(Hmisc::wtd.var(x, wts, na.rm=TRUE)) ) ) ))
      r$REGION <- zoneOverallName
      r$PCTILE <- rownames(r) # 1:100,'mean','std.dev'
    }
  } else {            ## ZONES
    r <- list()
    for (i in 1:length(unique(zone.vector))) {
      z <- unique(zone.vector)[i]
      if (is.null(wts)) {

        # ZONES BUT NO WEIGHTS ####

        r[[i]] <- data.frame(sapply(mydf[zone.vector==z,  ], function(x) pctiles.exact(x)))
        r[[i]] <- rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector==z,  ], function(x) mean(x, na.rm = TRUE)))))
        r[[i]] <- rbind(r[[i]], t(data.frame(std.dev = sapply(mydf[zone.vector==z,  ], function(x) sd(x, na.rm = TRUE)))))
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean','std.dev'
      } else {

        # ZONES AND WEIGHTS ####

        r[[i]] = data.frame(sapply(mydf[zone.vector==z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector==z]) ) )
        r[[i]] = rbind(r[[i]], t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) Hmisc::wtd.mean(x, wts[zone.vector==z], na.rm=TRUE) ) ) ))
        r[[i]] = rbind(r[[i]], t(data.frame(std.dev=sapply(mydf[zone.vector==z,  ], function(x) sqrt(Hmisc::wtd.var(x, wts[zone.vector==z], na.rm=TRUE) ) )) ))
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean','std.dev'
      }
    }
    # ZONE LOOP DONE
    r <- do.call(rbind, r)

  }
  # print(r)
  r <- data.frame(
    OBJECTID = 1:NROW(r),
    REGION = r$REGION,
    PCTILE = r$PCTILE,
    r[ , !(colnames(r) %in% c('REGION', 'PCTILE'))],
    stringsAsFactors = FALSE
  )
rownames(r) <- NULL
  if (!is.null(filename)) {
    write.csv(r, file = paste(filename, ".csv", sep = ""))
  }
  invisible(r)
}


