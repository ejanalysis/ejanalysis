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
#' @param dref optional - NONDEFAULTS ARE NOT YET CONFIRMED TO WORK HERE.
#'   This specifies reference group, default is 1 - d, meaning all people who are not in given group,
#'  so each D group is compared to all non-D people. This is why dlab has the default it does.
#'  But dref can be used to specify a single reference group used for all D analyzed.
#'  For the reference group to be the entire overall population, use dref = 1, and say dlab = c('avg in group', 'avg overall') for example.
#'  To use some other reference group, just set dref = a fraction of 1 that is the share of local pop that is in the reference group,
#'  as a vector as long as the number of places (number of rows in e or d).
#'  e.g., for non-Hispanic White alone to be the reference group,
#'  if the bg data.frame has a field called pctnhwa, specify dref = bg$pctnhwa,
#'  and specify dlab = c('mean in this group', 'mean in reference group') for example.
#' @param formulatype Optional, default is 'manual', which is like sum(x * wts) / sum(wts) with na.rm=T,
#'   or formulatype can be 'Hmisc' to use [Hmisc::wtd.mean()] (Hmisc::wtd.mean), or 'base' to use [weighted.mean()]
#' @param na.rm optional, default is TRUE. No effect if formulatype = manual (default). Not really the right results when na.rm = FALSE anyway.
#' @return numeric results as array
#' @seealso [RR()]
#' @examples
#'
#'  # See examples for [RR.table()] and [RR.means()] and [RR()]
#'
#'  ########################################  #
#'
#'  ##    if just using ejanalysis pkg test data:
#'  bg <- ejanalysis::bgtest
#'   enames <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score",
#'    "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
#'  dnames = c("pctlingiso", "pctlowinc")
#'  dnames.subgroups.count =  c("hisp", "nhwa", "nhba", "nhaiana",
#'    "nhaa", "nhnhpia", "nhotheralone", "nhmulti")
#'  dnames.subgroups.pct = c("pcthisp", "pctnhwa", "pctnhba", "pctnhaiana",
#'    "pctnhaa", "pctnhnhpia", "pctnhotheralone", "pctnhmulti")
#'
#'  ##    if EJAM pkg available:
#'  # bg <- as.data.frame(EJAM::blockgroupstats)
#'  # enames = EJAM::names_e
#'  # dnames = EJAM::names_d
#'  # dnames.subgroups.count = EJAM::names_d_subgroups_count
#'  # dnames.subgroups.pct  =  EJAM::names_d_subgroups
#'
#'  ##    if EJAM pkg not available and using ejscreen pkg data:
#'  # bg <- ejscreen::bg22
#'  # enames = ejscreen::names.e
#'  # dnames = ejscreen::names.d
#'  # dnames.subgroups.count = ejscreen::names.d.subgroups
#'  # dnames.subgroups.pct  =  ejscreen::names.d.subgroups.pct
#'
#'  ########################################  #
#'
#'  # stats on 1 Demographic group
#'  drop(RR.means(bg[ , enames], bg$pcthisp, bg$pop))
#'  # all E, all D
#'  x <- RR.means(bg[, enames], bg[, dnames], bg$pop)
#'  round(x[, 'ratio',],2)
#'  x[ , , 'traffic.score']
#'  x['pctlowinc', , ]
#'
#'  densities <- round(drop(RR.means(e = data.frame(pop.density = 1000 * bg$pop / bg$area),
#'   d = bg[, c(dnames, dnames.subgroups.pct)], pop = bg$pop, dlab = c('Avg pop density', 'Avg if not in this demog group'))),2)
#'  densities[order(densities[, 3], decreasing = T),]
#'
#' \dontrun{
#' # for ejscreen pkg data.frame
#' # population density of blockgroup, by demographic group
#' densities <- round(drop(RR.means(
#'   e = data.frame(pop.density = 1000 * bg$pop / bg$area),
#'   d = bg[, c(dnames, dnames.subgroups.pct)],
#'   pop = bg$pop,
#'   dlab = c('Avg pop density', 'Avg if not in this demog group'))),2)
#' # just 1 Envt factor
#'  drop(RR.means(bg[, "traffic.score"], bg[,c(dnames, dnames.subgroups.pct)], pop = bg$pop))
#'  # just 1 Demog group
#'  drop(RR.means(bg[, enames], bg[, "pctlowinc"], pop = bg$pop))
#'  # multiple E, multiple D
#'  RR.means(bg[, enames], bg[, c("pctlowinc", "pctunemployed")], pop = bg$pop)
#'  # All E, All D (a bit slow)
#'  x <- RR.means(bg[, enames], bg[, dnames], pop = bg$pop)
#'  t(round(x[,'ratio',],2))
#'
#'  # x <- ejanalysis::RR.means(
#'  #  bg[ , enames],
#'  #  bg[ , c(dnames, dnames.subgroups.pct)] / 100,
#'  #  bg$pop
#'  # )
#'  }
#'
#' @export
#'
RR.means <- function(e, d, pop, dref, dlab = c('group', 'not'), formulatype = 'manual', na.rm = TRUE) {

   warning('still debugging this - see example for traffic score lowinc vs not')

  if (missing(e) || missing(d) || missing(pop)) { stop('Missing e, d, &/or pop argument')}


  if (missing(dref)) {
    dref <- NULL # and the later function means.for.one.e will handle that
  } else {


    # need to make sure dref format will work
    ### but also need to check handled dref right in means.for.one.e() - note it can be vector or matrix of data.frame too !

    ## not finished checking that yet ... ***

    if (NROW(dref) > 1 && NCOL(dref) == 1) {
      if (NROW(dref) != NROW(d)) {
        stop('dref and d must have the same number of rows')
      }
      dref <- data.frame(ref = dref)
      # colnames(dref) <- "???"
    }
    if (NROW(dref) == 1 && NCOL(dref) == 1) {
      if (dref != 1) {
        stop('if only a single number is specified for dref, it must be 1 (just weighting by total population)')
      }
      dref <- data.frame(ref = rep(dref, NROW(d)))
    }
  }

  # for [,] The default is to drop if only one column is left, but not to drop if only one row is left.
  # so e or d could have been provided inadvertently as a vector instead of a 1-column data.frame

  # # will try this but not fixed to work yet:
  if (NCOL(e) == 1 || is.vector(e)) {
    e <- data.frame(e)
    colnames(e) <- 'e'
  }

  #####################################  #
  means.for.one.e <- function(e, d, pop, dref, dlabels, formulatype = 'manual', na.rm = TRUE) {

    # This function is for one envt factor,
    # for 1+ demog groups

    if (any(d > 1, na.rm = TRUE)) {
      message('d was expected to be fractions < 1, not 0-100...')
      drefmax = 100
    } else {
      drefmax = 1
    }
    if (is.null(dref)) {
      if (drefmax == 100) {
        warning("Assuming dref is 100 - d, since some d values were > 1, so assuming it was a percentage expressed as zero to 100")
      }
      dref <- drefmax - d
    }
    # if reference groups percents not specified, it is assumed to be everyone other than d...
    # If dref is everyone including the d group, that dilutes the RR (as when people compare demographics here to the US average overall.)
    # That makes a big difference if "here" is a large fraction of "overall."


    if (NCOL(d) == 1) {
      # for one e, one d:

      if (formulatype == 'manual') {

        # notok <-  unique(c(which(is.na(pop)), which(is.na(e)), which(is.na(d)), which(is.na(dref))))
        # ok <- setdiff(1:length(pop), notok)

        ## NOT TESTED YET:

        ok <- rowSums(cbind(is.na(pop) ,is.na(e),is.na(d), is.na(dref))) == 0

        # ok <- !(is.na(pop) || is.na(e) || is.na(d) || is.na(dref))
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
        # ok <- !(is.na(pop) || is.na(e) || is.na(rowSums(d)) )
        ok <- rowSums(cbind(is.na(pop) ,is.na(e),is.na(rowSums(d)) )) == 0
        if (NCOL(dref) > 1) {



          # **** not finished fixing the ok <-  lines




          ok <- rowSums(cbind(!ok, is.na(rowSums(dref)) )) == 0
          # ok <- ok & !is.na(rowSums(dref))


          drefok <- dref[ok, ]
        } else {

          ok <- rowSums(cbind(!ok, is.na(rowSums(dref)) )) == 0
          # ok <- ok & !is.na(rowSums(dref))


          drefok <- dref[ok, , drop = FALSE] # keep as a single column df
        }
        x <- cbind(
          colSums(pop[ok] * e[ok] * d[ok, ]) / colSums(pop[ok] *  d[ok, ]),
          colSums(pop[ok] * e[ok] * drefok) / colSums(pop[ok] *  drefok)
        )
      }

      if (formulatype == 'Hmisc') {
        x <- array(dim = c(NROW(e), NCOL(d), 2)) # *** not tested for right format
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
  #####################################  #
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
      FUN = list(ratio = function(x) x[1] / x[2])
    )

    return(myresults)
  }

  return(NA)
}
