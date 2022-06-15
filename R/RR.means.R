#' @title Relative Risk (RR) components - Means in demographic group and in rest of pop, based on Census data
#' @description Finds the mean indicator value in one demographic subgroup and mean in everyone else,
#'   based on data for each spatial unit such as for block groups or tracts.
#' @details This function requires, for each Census unit, demographic data on total population and percent in each demographic group,
#'   and some indicator(s) for each Census unit, such as health status, exposure estimates, or environmental health risk.
#'   For example, given population count, percent Hispanic, and ppm of ozone for each tract, this calculates the
#'   population mean tract-level ozone concentration among Hispanics and the same value among all non-Hispanics.
#'   The result is a table of means for a demographic subset, or for each of several groups and indicators.
#'   Each e (for environmental indicator) or d (for demographic percentage) is specified as a vector over small places like Census blocks or block groups
#'   or even individuals (ideally) but then d would be a dummy=1 for selected group and 0 for people not in selected group
#'   NOTE: could NA values cause a problem here?
#' @param e Vector or data.frame or matrix with 1 or more environmental indicator(s) or health risk level (e.g., PM2.5 concentration to which this person or place is exposed),
#'   one row per Census unit and one column per indicator.
#' @param d Vector or data.frame or matrix with 1 or more demog groups percentage (as fraction of 1, not 0-100!) of place that is selected demog group (e.g. percent Hispanic) (or d=1 or 0 if this is a vector of individuals)
#' @param pop Vector of one row per location providing population count of place (or pop=1 if this is a vector of individuals), to convert d into a count since d is a fraction
#' @param dlab optional character vector of two names for columns of output, default is c('group', 'not'), where the second refers to the reference group used.
#' @param dref optional specifies reference group, default is 1 - d, meaning all people who are not in given group,
#'  so each D group is compared to all non-D people. This is why dlab has the default it does.
#'  But dref can be used to specify a single reference group used for all D analyzed.
#'  For the reference group to be the entire overall population, use dref = 1, and say dlab = c('avg in group', 'avg overall') for example.
#'  To use some other reference group, just set dref = a fraction of 1 that is the share of local pop that is in the reference group,
#'  as a vector as long as the number of places (number of rows in e or d).
#'  e.g., for non-Hispanic White alone to be the reference group,
#'  if the bg data.frame has a field called pctnhwa, specify dref = bg$pctnhwa,
#'  and specify dlab = c('mean in this group', 'mean in reference group') for example.
#' @param formulatype Optional, default is 'manual', which is like sum(x * wts) / sum(wts) with na.rm=T,
#'   or formulatype can be 'Hmisc' to use \code{\link[Hmisc]{wtd.mean}} (Hmisc::wtd.mean), or 'base' to use \code{\link{weighted.mean}}
#' @param na.rm optional, default is TRUE. No effect if formulatype = manual (default). Not really the right results when na.rm = FALSE anyway.
#' @return numeric results as array
#' @template seealsoRR
#' @examples
#'  bg=ejanalysis::bgtest
 # namese = c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score",
 #  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
 # namesd = c("VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64")
 # namesdsub = c("pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia",  "pctnhotheralone", "pctnhmulti")
#'  drop(RR.means(bg[ , namese], bg$pcthisp, bg$pop))
#'  x=RR.means(bg[,namese], bg[,namesd], bg$pop)
#'  round(x[,'ratio',],2)
#'  x[ , ,'traffic.score']
#'  x['pctlowinc',,]
#'
#'  densities <- round(drop(RR.means(e = data.frame(pop.density=1000*bg$pop/bg$area),
#'   d = bg[, c(namesd, namesdsub)], pop = bg$pop, dlab = c('Avg pop density', 'Avg if not in this demog group'))),2)
#'  densities[order(densities[,3],decreasing = T),]
#'
#' @export
RR.means <- function(e, d, pop, dref, dlab = c('group', 'not'), formulatype = 'manual', na.rm = TRUE) {

  # warning('still debugging this')

  if (missing(e) || missing(d) || missing(pop)) { stop('Missing e, d, &/or pop argument')}
  if (any(d > 1, na.rm = TRUE)) {stop('d must be fractions < 1, not 0-100')}
  if (missing(dref)) {dref <- 1 - d} # if reference groups percents not specified, it is assumed to be everyone other than d

  # need to make sure dref format will work
  if (NROW(dref) > 1 & NCOL(dref) == 1) {
    if (NROW(dref) != NROW(d)) {
      stop('dref and d must have the same number of rows')
    }
    dref <- data.frame(ref = dref)
  }
  if (NROW(dref) == 1 & NCOL(dref) == 1) {
    if (dref != 1) {
      stop('if only a single number is specified for dref, it must be 1 (just weighting by total population)')
    }
    dref <- data.frame(ref = rep(dref, NROW(d)))
  }

  # # will try this but not fixed to work yet:
  if (NCOL(e) == 1) {
    e <- as.data.frame(e, drop=FALSE)
    colnames(e) <- 'e'
  }

  means.for.one.e <- function(e, d, pop, dref, dlabels, formulatype = 'manual', na.rm = TRUE) {

    # This function is for one envt factor,
    # for 1+ demog groups

    if (NCOL(d) == 1) {
      # for one e, one d:

      if (formulatype == 'manual') {
        ok <- !(is.na(pop) | is.na(e) | is.na(d) | is.na(dref))
        # would fail if dref were not just a vector which is a potential problem if >1 d used
        x <- cbind(
          (sum(pop[ok] * e[ok] *  d[ok]) / sum(pop[ok] *  d[ok])) ,
          (sum(pop[ok] * e[ok] * dref[ok]) / sum(pop[ok] * dref[ok]))
        )
      }
      # if (formulatype == 'manualold') {
      #   x <- cbind(
      #     (sum(pop * e *  d,     na.rm = na.rm) / sum(pop *  d,     na.rm = na.rm)) ,
      #     (sum(pop * e * (dref), na.rm = na.rm) / sum(pop * (dref), na.rm = na.rm))
      #   )
      # }
      if (formulatype == 'Hmisc') {x <- cbind(
        Hmisc::wtd.mean(e, weights = d    * pop, na.rm = na.rm),
        Hmisc::wtd.mean(e, weights = dref * pop, na.rm = na.rm)
      )
      }
      if (formulatype == 'base') {x <- cbind(
        stats::weighted.mean(e, w = d    * pop, na.rm = na.rm),
        stats::weighted.mean(e, w = dref * pop, na.rm = na.rm)
      )
      }

      colnames(x) <- dlabels
      return(x)

    } else {

      # for one e, multiple d: vectorized version
      # note I guess we expect dref to be same length as NROW(d),
      # and default is 1 - d but have not tested this to see if user-specified dref works

      if (formulatype == 'manual') {
        ok <- !(is.na(pop) | is.na(e) | is.na(rowSums(d)) )
        if (NCOL(dref) > 1) {
          ok <- ok & !is.na(rowSums(dref))
          drefok <- dref[ok, ]
        } else {
          ok <- ok & !is.na(rowSums(dref))
          drefok <- dref[ok, , drop = FALSE] # keep as a single column df
        }
        x <- cbind(
          colSums(pop[ok] * e[ok] * d[ok, ]) / colSums(pop[ok] *  d[ok, ]),
          colSums(pop[ok] * e[ok] * drefok) / colSums(pop[ok] *  drefok)
        )
      }

      if (formulatype == 'Hmisc') {
        x <- array(dim = c(NROW(e), NCOL(d), 2))# *** not tested for right format
        for (i in 1:NCOL(d)) {
          # *** not tested for right format, and if dref is not just a vector
          x[ , i, ] <- cbind(
            Hmisc::wtd.mean(e, weights = d[ , i] * pop, na.rm = na.rm),
            Hmisc::wtd.mean(e, weights = dref * pop, na.rm = na.rm)
          )
        }
      }
      if (formulatype == 'base')  {
        x <- array(dim = c(NROW(e), NCOL(d), 2))# *** not tested for right format
        for (i in 1:NCOL(d)) {
          # *** not tested for right format, and if dref is not just a vector
          x[ , i, ] <- cbind(
            stats::weighted.mean(e, w = d[ , i] * pop, na.rm = na.rm),
            stats::weighted.mean(e, w = dref * pop, na.rm = na.rm)
          )
        }
      }

      colnames(x) <- dlabels
      return(x)
    }
  }
  # -----------------

  # handle single envt factor  ---------- (but not if converted to 1 col matrix as done now!)

  if ( is.vector(e) && length(e) > 1 ) {
    myresults <- means.for.one.e(e, d, pop, dref = dref, dlabels = dlab, formulatype = formulatype, na.rm = na.rm)

    # dimnames(myresults) = list(d = colnames(d), group.or.not = dlab)
    dimnames(myresults) = list(e = 'e', d = colnames(d), group.or.not = dlab)

    # ** put the d dimension first, then the group.or.not dimension;
    # and add a column with ratio of group to not (not, ref, whatever)
    myresults <- addmargins(
      aperm(myresults, perm = c('d', 'group.or.not')),
      margin = 2,
      FUN = list(ratio = function(x) x[1]/x[2])
    )
    return(
      myresults
    )
    # could warn if length(d)!=length(e) & neither is an integer multiple of the other
    # same for length(pop)
  }

  # handle multiple envt factors ----------

  if ((is.data.frame(e) || is.matrix(e)) && length(e[ , 1]) > 1) {
    myresults <- array(
      NA,
      dim = c(NCOL(e), NCOL(d), 2),
      dimnames = list(e = colnames(e), d = colnames(d), group.or.not = dlab)
    )

    for (i in 1:NCOL(e)) {
      myresults[ i, , ] <- sapply(
        e[ , i, drop = FALSE],
        function(x) means.for.one.e(x, d, pop, dref = dref, dlabels = dlab, formulatype = formulatype, na.rm = na.rm)
      )
    }

    # ** put the d dimension first, then the group.or.not dimension then the e dimension;
    # and add a column with ratio of group to not (not, ref, whatever)
    myresults <- addmargins(
      aperm(myresults, perm = c('d', 'group.or.not', 'e')),
      margin = 2,
      FUN = list(ratio = function(x) x[1]/x[2])
    )

    return(myresults)
  }

  return(NA)
}
