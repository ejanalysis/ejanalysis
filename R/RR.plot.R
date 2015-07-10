#' @title Draw lineplot comparing RR values by group
#'
#' @description
#'  Draws a plot using relative risk information, one line per group
#' @param RRS Table as from \code{\link{RRS}} function, of relative risk by group, risk type, and zone (3 dimensions).
#' @param d Demographic group names to be found as names of first dim of RRS
#' @param e Environmental factor or risk type names to be found as names of second dim of RRS
#' @param zone Zone name to be found among names for third dim of RRS. Default is "NY"
#' @return draws a plot
#' @seealso \code{\link{RR}}
#' @examples #
#'   ###
#' @export
RR.plot <- function (RRS, d, e, zone='NY' ) {
  if (missing(d)) {
    data(names.dvars)
    d <- c(names.d, names.d.subgroups)
  }
  plot(    round(RRS[d[1], e, zone] / RRS[d[1], e, 'USA'], 2) , main=paste('RR by group for ', zone, ' for ', e, sep=''), xlab=d)

  for (i in 2:length(d)) {

    lines( round(RRS[d[i], e, zone] / RRS[d[i], e, 'USA'], 2) )

  }
}
