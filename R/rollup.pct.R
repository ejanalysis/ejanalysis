#' @title Calculate a/b for each Subset
#' @description  Uses data.table package to quickly calculate
#'   ratio of a/b within each subset of a dataset (e.g., by zone).
#'   This will be superseded by [rollup()] once that is completed.
#' @param a Required numeric vector, numerator
#' @param b Required numeric vector, denominator. Same length as a.
#' @param zone Optional, vector to group by. Same length as a and b.
#' @return Returns a table with a/b calculated within each zone.
#' @seealso [make.bin.pctile.cols()] and [assign.pctiles()]
#' @examples
#'   pre1960=1:100; builtunits=rep(c(10, 100),50); zone=rep(c('NY','MA'),50)
#'   rollup.pct(a,b,zone)
#' @export
rollup.pct <- function(a, b, zone) {
  library(data.table)
  warning('not fully tested and will be superseded by rollup() when that uses data.table')
  dat <- data.table::data.table(a=a, b=b, zone=zone, stringsAsFactors=FALSE)
  return( dat[ , pct= sum(a) / sum(b), by= zone] )


  # specific example without this function:
  # get bg dataset via   # load('')
  # require(data.table)
  # pb=data.table(bg[ , c('ST', 'pre1960', 'builtunits')], key = 'ST')
  # head(pb)
  # pb[ , floor(100*sum(pre1960)/sum(builtunits))] # USA %
  # x=pb[ , floor(100*sum(pre1960)/sum(builtunits)), by=ST]
  # x[ order(V1), ]
}
