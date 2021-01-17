#' @title Just print Relative Risks (RR) info by demog group by risk type by zone
#' @description Currently does not do anything other than print. Takes zone-specific RRS values and prints them for viewing.
#'   Just another way to specify which subset of the RR array you want to view.
#' @param rrs A three dimensional array that is created by \code{\link{RR}} and has format like RRS[d, e, zone]
#' @param d Optional. Vector of names of demographic groups that must be subset of first dimension of RRS. Default is all.
#' @param e Optional. Vector of names of environmental risk indicators that must be subset of second dimension of RRS. Default is all.
#' @param zone Optional. Vector of zone names that must be subset of names of third dimension of RRS. Default is all.
#' @examples
#'  t(round(RR.table.view(
#'    RR.table(bgtest, names.e[1:3], names.d[3:4], 'pop', Zcolname = 'statename'),
#'    d = 'pctlowinc'
#'   ), 2))
#' @return prints RRS[d, e, zone]
#' @template seealsoRR
#' @export
RR.table.view <- function(x, d = dimnames(x)[[1]], e = dimnames(x)[[2]], zone = dimnames(x)[[3]]) {
  print(x[d, e, zone] )
}
