#' @title Sort table of Relative Risk results by zone by group by envt risk factor
#' @description Sort table of Relative Risk results by zone by group by envt risk factor
#' @param x Array of RR values from \code{\link{RR}} in array of 3 dimensions: x[Dnames, Enames, Zcolnames]
#' @param decreasing default is TRUE, defines how to sort second and third dimensions (envt risk and demog group)
#' @return Sorted version of x array of 3 dimensions: RRS[Dnames, Enames, Zcolnames]
#'   Returns an array with
#'   one demographic group per row,
#'   one environmental risk indicator per column, and
#'   third dimension for which zone (e.g., which US State)
#' @template seealsoRR
#' @keywords EJ
#' @examples
#' RRS.US  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop')
#' RRS.ST  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='ST')
#' RRS <- RR.table.add(RRS.ST, RRS.US)
#' RRS[ 'pctlowinc', , ]
#' RRS[ , , 'CA']
#' RRS[ , 'pm', ]
#'
#' RR.table.sort(RRS)
#'
#' RRS.REGION  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='REGION')
#' RRS2 <- RR.table.add(RRS, RRS.REGION)
#' RRS2[ , , '8']
#' @export
RR.table.sort <- function(x, decreasing=TRUE) {
  # Takes one table, a 2-D slice of results of RR(), and sorts by col and by row to show highest RR values at top left
  # Example: RRS <- RR()
  x <- x[ order(x[ , 1], decreasing=TRUE), ]
  x <- x[ , order(x[1, ], decreasing=TRUE) ]
  return(x)
}

