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
#'  @param formulatype optional, default is 'manual', which is like sum(x * wts) / sum(wts) with na.rm=T,
#'   or formulatype can be 'Hmisc' to use \code{\link[Hmisc]{wtd.mean}} (Hmisc::wtd.mean), or 'base' to use \code{\link{weighted.mean}}
#' @param na.rm optional, default is TRUE. No effect if formulatype = manual (default). Not really the right results when na.rm = FALSE anyway.
#' @return numeric results as array
#' @template seealsoRR
#' @examples
#' # RR.means( bg$proximity.rmp, bg$pcthisp, bg$pop)
#' # RR.means(bg[ , names.e], bg$pcthisp, bg$pop)
#' # RR.means( bg$proximity.rmp, cbind(bg[ , names.d.subgroups],1), bg$pop)
#' mydat <- structure(list(state = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L,
#' 8L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L,
#' 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L,
#' 35L, 36L, 37L, 38L, 39L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L,
#' 49L, 50L, 51L, 52L), .Label = c("Alabama", "Alaska", "Arizona",
#' "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
#' "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho",
#' "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
#' "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
#' "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
#' "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
#' "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico",
#' "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
#' "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
#' "Wisconsin", "Wyoming"), class = "factor"), pcthisp = c(0.0381527239296627,
#' 0.056769492321473, 0.296826116572835, 0.0635169313105461, 0.375728960493789,
#' 0.206327251656949, 0.134422275491411, 0.0813548250199138, 0.2249082771481,
#' 0.0878682317249484, 0.0901734019211436, 0.111947738331921, 0.158094676641822,
#' 0.0599941716405598, 0.0495552961203499, 0.104741084665558, 0.0301921562004411,
#' 0.0425162900517816, 0.0129720920573869, 0.0816325860392955, 0.0960897601513277,
#' 0.0442948677533508, 0.0470583828855611, 0.0264973278249911, 0.0354626134972627,
#' 0.0292535716628734, 0.0914761950105784, 0.265451497002445, 0.0283535007142456,
#' 0.177132117215957, 0.463472498001496, 0.176607017430808, 0.0834317084560556,
#' 0.0209906647364226, 0.0307719359181436, 0.0883052970054721, 0.117261303415395,
#' 0.0568442805511265, 0.124769233546578, 0.0503041778042313, 0.0279186292931113,
#' 0.0454229079840698, 0.376044616311455, 0.129379195461843, 0.0151111594281676,
#' 0.0788112971314249, 0.111945098129999, 0.0119028512046327, 0.0589405823830593,
#' 0.0893971780534219), pop = c(3615, 365, 2212, 2110, 21198, 2541,
#' 3100, 579, 8277, 4931, 868, 813, 11197, 5313, 2861, 2280, 3387,
#' 3806, 1058, 4122, 5814, 9111, 3921, 2341, 4767, 746, 1544, 590,
#' 812, 7333, 1144, 18076, 5441, 637, 10735, 2715, 2284, 11860,
#' 931, 2816, 681, 4173, 12237, 1203, 472, 4981, 3559, 1799, 4589,
#' 376), murder = c(15.1, 11.3, 7.8, 10.1, 10.3, 6.8, 3.1, 6.2,
#' 10.7, 13.9, 6.2, 5.3, 10.3, 7.1, 2.3, 4.5, 10.6, 13.2, 2.7, 8.5,
#' 3.3, 11.1, 2.3, 12.5, 9.3, 5, 2.9, 11.5, 3.3, 5.2, 9.7, 10.9,
#' 11.1, 1.4, 7.4, 6.4, 4.2, 6.1, 2.4, 11.6, 1.7, 11, 12.2, 4.5,
#' 5.5, 9.5, 4.3, 6.7, 3, 6.9), area = c(50708, 566432, 113417,
#' 51945, 156361, 103766, 4862, 1982, 54090, 58073, 6425, 82677,
#' 55748, 36097, 55941, 81787, 39650, 44930, 30920, 9891, 7826,
#' 56817, 79289, 47296, 68995, 145587, 76483, 109889, 9027, 7521,
#' 121412, 47831, 48798, 69273, 40975, 68782, 96184, 44966, 1049,
#' 30225, 75955, 41328, 262134, 82096, 9267, 39780, 66570, 24070,
#' 54464, 97203), temp = c(62.8, 26.6, 60.3, 60.4, 59.4, 45.1, 49,
#' 55.3, 70.7, 63.5, 70, 44.4, 51.8, 51.7, 47.8, 54.3, 55.6, 66.4,
#' 41, 54.2, 47.9, 44.4, 41.2, 63.4, 54.5, 42.7, 48.8, 49.9, 43.8,
#' 52.7, 53.4, 45.4, 59, 40.4, 50.7, 59.6, 48.4, 48.8, 50.1, 62.4,
#' 45.2, 57.6, 64.8, 48.6, 42.9, 55.1, 48.3, 51.8, 43.1, 42)), .Names = c("state",
#' "pcthisp", "pop", "murder", "area", "temp"), class = "data.frame", row.names = c(NA,
#' -50L))
#'
#' RR.means(mydat$area, mydat$pcthisp, mydat$pop)
#'
#' RR.means(mydat$pcthisp, mydat$pcthisp, mydat$pop)
#'
#' # CHECK FORMATS OF OUTPUTS: *** WORK IN PROGRESS
#'
#' #cbind(mymeans=RR.means(data.frame(d1=bg$pcthisp, d2=1-bg$pcthisp), bg$pcthisp, bg$pop))
#' #RR.means(bg[ , names.e], bg$pctlowinc, bg$pop)
#' #sapply(bg[ , names.d], function(z) RR.means(bg[ , names.e], z, bg$pop) )
#' @export
RR.means <- function(e, d, pop, dref, dlab = c('group', 'not'), formulatype = 'manual', na.rm = TRUE) {

  warning('still debugging this')

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
  # if (NCOL(e) == 1) {
  #   e <- as.matrix(e)
  #   colnames(e) <- 'e'
  # }

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
        weighted.mean(e, w = d    * pop, na.rm = na.rm),
        weighted.mean(e, w = dref * pop, na.rm = na.rm)
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
            weighted.mean(e, w = d[ , i] * pop, na.rm = na.rm),
            weighted.mean(e, w = dref * pop, na.rm = na.rm)
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
