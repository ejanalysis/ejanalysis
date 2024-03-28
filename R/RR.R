#' @title Relative Risk (RR) by demographic group by indicator based on Census data
#' @description Finds the ratio of mean indicator value in one demographic subgroup to mean in everyone else,
#'   based on data for each spatial unit such as for block groups or tracts.
#' @details This function requires, for each Census unit, demographic data on total population and percent in each demographic group,
#'   and some indicator(s) for each Census unit, such as health status, exposure estimates, or environmental health risk.
#'   For example, given population count, percent Hispanic, and ppm of ozone for each tract, this calculates the ratio
#'   of the population mean tract-level ozone concentration among Hispanics divided by the same value among all non-Hispanics.
#'   The result is a ratio of means for two demographic groups, or for each of several groups and indicators.
#'   Each e (for environmental indicator) or d (for demographic percentage) is specified as a vector over small places like Census blocks or block groups
#'   or even individuals (ideally) but then d would be a dummy=1 for selected group and 0 for people not in selected group
#'
#'   note: this currently does not use rrf() & rrfv() but perhaps it would be faster if it did? but rrfv not tested for multiple demog groups \cr
#'
#'    VERIFY/TEST THIS: REMOVES PLACES WITH NA in any one or more of the values used (e, d, pop, dref) in numerators and denominators. \cr
#'
#'    Note also that THIS REMOVES NA VALUES FOR one e factor and not for another,
#'   so results can use different places & people for different e factors
#' @param e Vector or data.frame or matrix with 1 or more environmental indicator(s) or health risk level
#'   (e.g., PM2.5 concentration to which this person or place is exposed),
#'   one row per Census unit and one column per indicator.
#' @param d Vector or data.frame or matrix with 1 or more demog groups percentage
#'   (as fraction of 1, not 0-100!)
#'   of place that is selected demog group (e.g. percent Hispanic)
#'   (or d=1 or 0 per row if this is a vector of individuals)
#' @param pop Vector of one row per location providing population count of place
#'   (or pop=1 if this is a vector of individuals),
#'   to convert d into a count since d is a fraction
#' @param dref Optional vector specifying a reference group for RR calculation
#'   by providing what percentage (as fraction of 1, not 0-100!) of place that is individuals in the reference group
#'   (or dref= vector of ones and zeroes if this is a vector of individuals)
#' @param na.rm Optional, logical, TRUE by default. Specify if NA values should be removed first.
#' @return numeric results as vector or data.frame
#'
#' @seealso
#' - [RR()] to calculate overall disparity metric as relative risk (RR), ratio of mean environmental indicator values across demographic groups
#' - [RR.table()] to create 3-D table of RR values, by demographic group by environmental indicator by zone
#' - [RR.table.sort()] to sort existing RR table
#' - [RR.table.add()] to add zone(s) to existing RR table
#' - [write.RR.tables()] to write a file with a table or RR by indicator by group
#' - [pop.ecdf()] to compare plots of cumulative frequency distribution of indicator values by group
#' - [RR.cut.if.gone()] to find local contribution to RR
#' - [RR.if.address.top.x()] to find how much RR would change if top-ranked places had different conditions
#' - [ej.added()] to find EJ Index as local contribution to sum of EJ Indexes
#' - [ej.indexes()] for local contribution to a variety of overall disparity metrics such as excess risk
#'
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
#'  x <- ejanalysis::RR(e = bg[, enames], d = bg[, dnames], pop = bg$pop)
#'  round(x, 2)
#'  t(round(x, 2))
#'
#'  sapply(bg[ , dnames], function(z) round(RR(bg[ , enames], z, bg$pop), 2))
#'
#' ejanalysis::RR(bg$pcthisp, bg$pcthisp, bg$pop)
#'  #  Avg Hispanic persons local percent Hispanic (their blockgroup)
#'  #  is 4x as everyone elses on avg,
#'  #  but avg low income persons local percent low income
#'  #  is only 1.8x as high as everyone elses.
#'
#'  cbind(RR = RR(
#'      e = data.frame(
#'        local_pct_hispanic = bg$pcthisp,
#'        local_pct_lowincome = bg$pctlowinc),
#'      d = cbind(
#'        Ratio_of_avg_among_hispanics_to_avg_among_nonhispanics = bg$pcthisp,
#'        avg_among_lowinc_vs_rest_of_pop = bg$pctlowinc),
#'      bg$pop))
#'
#' @export
#'
RR <- function(e, d, pop, dref, na.rm=TRUE) {

  if (missing(e) || missing(d) || missing(pop)) { stop('Missing e, d, &/or pop argument')}
  if (any(d > 1, na.rm = TRUE)) {
    message('d was expected to be fractions < 1, not 0-100...')
    drefmax = 100
  } else {
    drefmax = 1
  }
  if (missing(dref)) {
    if (drefmax == 100) {
      warning("Assuming dref is 100 - d, since some d values were > 1, so assuming it was a percentage expressed as zero to 100")
    }
    dref <- drefmax - d
  } # if reference groups percents not specified, it is assumed to be everyone other than d...
  # If dref is everyone including the d group, that dilutes the RR (as when people compare demographics here to the US average overall.)
  # That makes a big difference if "here" is a large fraction of "overall."

  ################################################################ #
  ################################################################ #
  RR.for.one.e <- function(e, d, pop, dref, na.rm=TRUE) {

    # for one envt factor,
    # for 1+ demog groups


    if (is.vector(d)) {
      # for one e, one d:
      #  **** put in +e-e  +d-d and  +dref-dref because I THINK WHERE e IS NA and pop and d are valid, this otherwise would remove those from numerator of group's risk but not from denominator!!!
      (  sum(pop * e *  d + dref - dref, na.rm = na.rm) / sum(pop *  d     + e - e + dref - dref, na.rm = na.rm)) /
        (sum(pop * e * (dref) + d - d,   na.rm = na.rm) / sum(pop * (dref) + e - e + d - d,       na.rm = na.rm))
      # weighted.mean function in base should give same result but have not confirmed true for strange cases like NA values etc.
      # weighted.mean(e, pop * d +dref-dref, na.rm=na.rm) / weighted.mean(e, pop * d   +e-e +dref-dref, na.rm=na.rm)
      #(sum(pop * e *  d,     na.rm=na.rm) / sum(pop *  d,     na.rm=na.rm)) /
      #(sum(pop * e * (dref), na.rm=na.rm) / sum(pop * (dref), na.rm=na.rm))
    } else {
      # for one e, multiple d: vectorized version  ( but also see analyze.stuff::wtd.colMeans()  )
      #  **** put in +e-e  +d-d and  +dref-dref because I THINK WHERE e IS NA and pop and d are valid, this otherwise would remove those from numerator of group's risk but not from denominator!!!
      (  colSums(pop * e *  d + dref - dref, na.rm = na.rm) / colSums(pop *  d     + e - e + dref - dref, na.rm = na.rm)) /
        (colSums(pop * e * (dref) + d - d,   na.rm = na.rm) / colSums(pop * (dref) + e - e + d - d,       na.rm = na.rm))
    }
  }
  ################################################################ #
  ################################################################ #

  # handle single envt factor
  if ( is.vector(e) && length(e) > 1 ) {
    # 1 E
    x <- RR.for.one.e(e = e, d = d, pop = pop, dref = dref)
    if (NCOL(d) > 1) {
      # 1 E but multiple D
      x <- array(x, dim = NCOL(d), dimnames = list(d = names(x)))
    } else {
      # # 1 E and 1 D
      # no dimnames
    }
    return(x)
    # could warn if length(d)!=length(e) & neither is an integer multiple of the other
    # same for length(pop)
  }

  # handle multiple envt factors
  if ((is.data.frame(e) || is.matrix(e)) && length(e[ , 1]) > 1) {
    myresults <- sapply(e, function(x) RR.for.one.e(x, d, pop, dref))
    if (NCOL(d) > 1) {
      # multiple E multiple D
      names(dimnames(myresults)) <- c('d', 'e') #

    } else {
      # multiple E but 1 D
      myresults <- array(myresults, dim = NCOL(e), dimnames = list(e = names(myresults)))
    }
    return(myresults)
  }

  return(NA)
}
