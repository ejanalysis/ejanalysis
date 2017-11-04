#' @title Demographic groups with worst RR
#' @description View one key part of table of Relative Risk results by zone by group by envt risk factor
#' @param d not used. name of demographic field with percent (fraction) of population that is in each given demographic group, in dimnames(rrs)[[1]]
#' @param e name of environmental risk factor in dimnames(rrs)[[2]]
#' @param zone name of zone such as 'USA' or 'NY' found in dimnames(rrs)[[3]]
#' @param n worst 10 by default
#' @param digits round to 2 by default
#' @return matrix
#' @template seealsoRR
#' @keywords EJ
#' @examples \dontrun{
#'   data(bgtest, package = 'ejanalysis')
#'   RRS.US  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop')
#'   RRS.ST  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop',
#'                   Zcolname = 'ST')
#'   RRS <- RR.table.add(RRS.ST, RRS.US)
#'   worstd(RRS.US)
#' }
#' @export
worstd <- function(rrs, d = 'pctlowinc', e = 'pm', zone = 'USA', n = 10, digits = 2) {

  cat(paste('\n\nGroups with worst RR for this zone (', zone, ') and Envt Indicator (', e,'):\n', sep = ''))
  z <- cbind(round(rrs[ , e, zone] , digits))
  head(cbind(RR = z[order(z[ , 1], decreasing = TRUE), ]), n)
}
