#' @title See which group and risk indicator (in zone) have max values in RR table
#' @description See which Demographic group has highest RR for each Envt risk indicator,
#'   and which Envt risk indicator has highest RR for each Demographic group,
#'   within each zone (USA, etc.),
#'   in table of Relative Risk results by zone by group by envt risk factor.
#' @param x Array results from [RR.table()] or related function
#' @param by optional vector (numeric or character), specifies which of three dimensions should be shown.
#'  Default is 1, meaning dimension 1 which is demographic indicator.
#' @param digits optional number of digits to round to, default is 3.
#' @return The full x array, except only the first two elements of the specified dimension,
#'   which should be the two elements with the max number and text saying which name is that max.
#' @seealso [RR()]
#' @export
RR.table.max <- function(x, by = 1, digits = 3) {

  warning('not working yet')


  # this works for 2 of the 3 dimensions:
  #
  # RRS3 <- addmargins(round( RRS2[ , , 'USA'], 3), FUN = list(max = function(v) names(v[-1])[which.max(v[-1])]))
  # RRS3 <- RRS3[ c('max', rownames(RRS2)) , c('max', colnames(RRS2))]
  #

  # this does work just for one dim:
  #
  # n=1; addmargins(RRS.US, n, list(max = function(x) dimnames(RRS.US[ , , 'USA'])[[n]][1+which.max(x[-1])]))


  ## what do we need/ want? JUST
  #  max(E) by D by Z, shown in ED table (1 zone) and EZ table (1 Demog), not in DZ table slice (just 1 E).
  #  max(D) by E by Z, shown in DE & DZ slices
  #  max(Z) by E by D, shown in ZE & ZD slices
  #
  # Those can tell you,
  #
  # In each zone,
  #   what is worst e for each d? max(e) by z by d  E,ZD
  #   what is worst d for each e? max(d) by z by e  D,ZE
  #
  # for just this d,
  #   what is worst zone for each e? max(z) by e by d  Z,ED
  #   what is worst e in each zone? max(e) by z by d - already above
  #
  # for just this e,
  #   what is worst d in each zone? max(d) by z by e - already above
  #   what is worst zone for each d? max(z) by e by d - already above


  if (by == 1 | by == names(x)[1]) {
    for (myd in dimnames(x)[[1]]) {
      x[myd, , ] <- addmargins(round(x[myd, , ], digits), FUN = list(max = function(v) names(v[-1])[which.max(v[-1])]))
      x[myd, , ] <- x[myd, c('max', rownames(x[myd, , ])) , c('max', colnames(x[myd, , ]))]
    }
    return(x[1:2, , ])
  }

  if (by == 2 | by == names(x)[2]) {
    for (mye in dimnames(x)[[2]]) {
      x[ , mye, ] <- addmargins(round(x[ , mye, ], digits), FUN = list(max = function(v) names(v[-1])[which.max(v[-1])]))
      x[ , mye, ] <- x[c('max', rownames(x[ , mye, ])), mye, c('max', colnames(x[ , mye, ]))]
    }
    return(x[ , 1:2, ])
  }

  if (by == 3 | by == names(x)[3]) {
    for (myzone in dimnames(x)[[3]]) {
      x[ , , myzone] <- addmargins(round(x[ , , myzone], digits), FUN = list(max = function(v) names(v[-1])[which.max(v[-1])]))
      x[ , , myzone] <- x[c('max', rownames(x[ , , myzone])) , c('max', colnames(x[ , , myzone])), myzone]
    }
    return(x[ , , 1:2])
  }
}
