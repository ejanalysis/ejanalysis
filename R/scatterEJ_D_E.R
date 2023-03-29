#' scatterplot EJScreen Demog vs Envt vs EJ Index
#' See one point per blockgroup, x=demog pctile, y=envt pctile, color=EJ pctile
#' @param d vector percentile demographic index
#' @param e vector percentile environmental indicator
#' @param ejbin vector bin number 1 through N (bins of percentiles) for EJ Index for e
#' @param ename friendly name for graphic, optional
#' @param legendtitle optional
#' @param colors11 optional
#' @param main optional
#' @param mylegend vector of text labels for the N bins
#' @param ... passed to plot (but not to points for the various colors subsets)
#' @examples  \dontrun{
#'  if (require(ejscreen)) {
#'  bg = ejscreen::bg22
#'  dd = bg$pctile.VSI.eo
#'  ee = bg$pctile.traffic.score
#'  jj = bg$bin.EJ.DISPARITY.traffic.score.eo
#'  scatterEJ_D_E(d=dd, e = ee, ejbin = jj, ename = "Traffic Score")
#'  }
#'
#'  if (require(EJAM)) {
#'  bg = data.table(copy(EJAM::blockgroupstats))
#'  ee = with(EJAM::blockgroupstats, as.vector(100 * make.pctile.cols(
#'    traffic.score, as.df = F)) )
#'  dd = with(blockgroupstats, as.vector(100 * make.pctile.cols(
#'    VSI.eo, as.df = F)))
#'  jj = with(blockgroupstats, as.vector(1 * make.bin.cols( as.vector(make.pctile.cols(
#'    EJ.DISPARITY.traffic.score.eo, as.df = FALSE)), as.df = F)))
#'  scatterEJ_D_E(d=dd, e = ee, ejbin = jj, ename = "Traffic Score")
#'  }
#' }
#' @return Nothing. Draws a scatter plot.
#' @export
#'
scatterEJ_D_E <- function(d, e, ejbin, ename='',
                          legendtitle='Percentiles of EJ Index (with yellow/orange/red as in the maps)',
                          colors11 = c( 'white', 'lightgray', 'lightgreen', 'darkgray', 'purple', 'black', 'darkblue', 'lightblue', 'yellow', 'orange', 'red'),
                          main='What combinations of E and D result in EJ Index being in various bins
                          so that place is shown on maps as in a given color-coded EJ Index percentile bin',
                          mylegend =  c('<10', '10-20','20s', '30s', '40s', '50-60', '60s', '70s', '80-90', '90-95', '>95'),  ...) {
  cx <- colors11

  plot(x=d, y=e, pch='.', col='white',
       xlim = c(0,100), ylim = c(0,100),
       asp=1,
       main = main,
       xlab = 'Demographic %ile',
       ylab = paste('Environmental %ile', ename, sep=" "), ...)

  for (i in seq_along(cx)) {
    points(d[ejbin == i ], e[ejbin == i ], pch='.', col=cx[i])
  }
  legend(x = 'bottom', x.intersp=0.5,  legend = mylegend, horiz = TRUE, fill = cx,
  title = legendtitle)
}
