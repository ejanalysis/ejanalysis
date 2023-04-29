#' @title Places with worst RR
#' @description View one key part of table of Relative Risk results by group by envt risk factor
#' @param rrs Required. This has to be the output of one of the functions like [RR.table()]
#' @param d name of demographic field with percent (fraction) of population that is in each given demographic group, in dimnames(rrs)[[1]]
#' @param e name of environmental risk factor in dimnames(rrs)[[2]]
#' @param n worst 10 by default
#' @param digits round to 2 by default
#' @return matrix
#' @template seealsoRR
#' @examples  \dontrun{
#'   data(bgtest, package = 'ejanalysis')
#'   RRS.US  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop')
#'   RRS.ST  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop',
#'                   Zcolname = 'ST')
#'   RRS <- RR.table.add(RRS.ST, RRS.US)
#'   worstplaces(RRS.US)
#' }
#' @export
worstplaces <- function(rrs, d = 'pctlowinc', e = 'pm', n = 10, digits = 2) {
  # zone = 'USA',
  cat(paste('\n\nZones with worst RR for this Demog group (', d, ') and Envt Indicator (', e,'):\n', sep = ''))
  z <- cbind(round(rrs[  d, e, ] , digits))
  head(cbind(RR = z[order(z[ , 1], decreasing = TRUE), ]), n)
}
