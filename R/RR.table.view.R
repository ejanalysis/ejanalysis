#' @title Print Relative Risks (RR) info by demog group by risk type by zone
#' @description Takes zone-specific RRS values and prints them for viewing.
#' @param RRS A three dimensional array that is created by \code{\link{RR}} and has format like RRS[d, e, zone]
#' @param d Vector of names of demographic groups that must be subset of first dimension of RRS
#' @param e Vector of names of environmental risk indicators that must be subset of second dimension of RRS
#' @param zone Vector of zone names that must be subset of names of third dimension of RRS
#' @return prints RRS[d, e, zone]
#' @template seealsoRR
#' @export
RR.table.view <- function(RRS, d, e, zone) {
  print( RRS[d, e, zone] )
}
