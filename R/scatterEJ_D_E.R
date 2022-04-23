#' scatterplot EJScreen Demog vs Envt vs EJ Index
#'
#' @param d vector percentile demographic index
#' @param e vector percentile environmental indicator
#' @param ejbin vector bin number 1 through N (bins of percentiles) for EJ Index for e
#' @param ename friendly name for graphic, optional
#' @param legendtitle optional
#' @param colors11 optional
#' @param main optional
#' @param mylegend vector of text labels for the N bins
#' @param ... passed to plot (but not to points for the various colors subsets)
#'
#' @return Nothing. Draws a scatter plot.
#' @export
#'
scatterEJ_D_E <- function(d=bg21$pctile.VSI.eo, e=bg21$pctile.traffic.score,
                          ejbin=bg21$bin.EJ.DISPARITY.traffic.score.eo, ename='',
                          legendtitle='Percentiles of EJ Index (with yellow/orange/red as in the maps)',
                          colors11 = c( 'white', 'lightgray', 'lightgreen', 'darkgray', 'purple', 'black', 'darkblue', 'lightblue', 'yellow', 'orange', 'red'),
                          main='EJ Index percentile bins', mylegend =  c('<10', '10-20','20s', '30s', '40s', '50-60', '60s', '70s', '80-90', '90-95', '>95'),  ...) {
  cx <- colors11

  plot(x=d, y=e, pch='.', col='white',
       xlim = c(0,100), ylim = c(0,100),
       asp=1,
       main = main,
       xlab = 'Demographic %ile',
       ylab = 'Environmental %ile', ...)

  for (i in seq_along(cx)) {
    points(d[ejbin == i ], e[ejbin == i ], pch='.', col=cx[i])
  }

  legend(x = 'bottom', x.intersp=0.5,  legend = mylegend, horiz = TRUE, fill = cx,
  title = legendtitle)
  # scatterEJ_D_E(e=bg21$pctile.cancer,ename = 'cancer',ejbin = bg21$pctile.EJ.DISPARITY.cancer.eo)
}
