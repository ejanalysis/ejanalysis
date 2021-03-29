#' @title Sort table of Relative Risk results by zone by group by envt risk factor
#' @description Sort table of Relative Risk results by zone by group by envt risk factor
#' @param x Array of RR values from \code{\link{RR}} in array of 3 dimensions: x[Dnames, Enames, Zcolnames]
#' @param margins optional numeric vector of which dimensions to sort, default is all 3, so margins = c(1, 2, 3) by default
#' @param decreasing default is TRUE, defines how to sort (note all specified dimensions are sorted the same way by this function)
#' @return Sorted version of x array of 3 dimensions: RRS[Dnames, Enames, Zcolnames]
#'   Returns an array with
#'   one demographic group per row,
#'   one environmental risk indicator per column, and
#'   third dimension for which zone (e.g., which US State)
#' @template seealsoRR
#' @examples
#' RRS.US  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct),
#'  popcolname='pop')
#' RRS.ST  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct),
#'  popcolname='pop', Zcolname='ST')
#' RRS <- RR.table.add(RRS.ST, RRS.US)
#' RRS[ 'pctlowinc', , ]
#' RRS[ , , 'CA']
#' RRS[ , 'pm', ]
#'
#' RR.plot(RR.table.sort(RRS))
#' RR.table.sort(RRS, 1)
#'
#' RRS.REGION  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct),
#'  popcolname='pop', Zcolname='REGION')
#' RRS2 <- RR.table.add(RRS, RRS.REGION)
#' RRS2[ , , '8']
#' @export
RR.table.sort <- function(x, margins = c(1, 2, 3), decreasing = TRUE) {
  # Takes one table, a 3-D slice of results of RR(), and
  # sorts by col and by row to show highest RR values at top left
  if ('max.zone' %in% dimnames(RRS.US)[3]) {
    keyzone <- 'max.zone'
  } else {
    if ('USA' %in% dimnames(RRS.US)[3]) {
      keyzone <- 'USA'
    } else {
      keyzone <- '' # not sure what to use in this case
    }
  }

  if (1 %in% margins) {
    if ('max.zone' %in% dimnames(x)$zone) {
      # sort D indicators by max.E max.zone
      x <- x[order(x[ , 'max.E', 'max.zone'], decreasing = TRUE), , ]
    } else {
      if ('USA' %in% dimnames(x)$zone) {
        # sort D indicators by max.E max.zone
        x <- x[order(x[ , 'max.E', 'USA'], decreasing = TRUE), , ]
      } else {
        if (NROW(x[1,1,]) == 1) {
          # not sure which zone to use when sorting D but assume want to use the one existing zone if only 1
          x <- x[ , order(x[, 'max.E', 1], decreasing = TRUE), ]
        }
      }
    }
  }
  if (2 %in% margins) {
    if ('max.zone' %in% dimnames(x)$zone) {
      # sort E indicators by max.D max.zone
      x <- x[ , order(x['max.D', , 'max.zone'], decreasing = TRUE), ]
    } else {
        if ('USA' %in% dimnames(x)$zone) {
          x <- x[ , order(x['max.D', , 'USA'], decreasing = TRUE), ]
        } else {
          if (NROW(x[1,1,]) == 1) {
            # not sure which zone to use when sorting E but assume want to use the one existing zone if only 1
            x <- x[ , order(x['max.D', , 1], decreasing = TRUE), ]
          }
        }
      }
  }

  if (3 %in% margins) {
    # assumes max.E and max.D are names found here
    # sort zones by max.E max.D
    if (!('max.D' %in% dimnames(x)[2]) & ('max.E' %in% dimnames(x)[3])) {warning('max.E and max.D as names both expected but not seen here')}
    x <- x[ , ,order(x['max.D', 'max.E', ], decreasing = TRUE)]
  }

  return(x)
}

