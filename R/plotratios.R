#' create plot of ratios indicating disparity, from sim_riskbydistance output
#'
#' @param x output from sim_riskbydistance
#'
#' @export
#'
#' @seealso sim_riskbydistance
sim_plotratios <- function(x) {
  plot(x$dist, x$riskratio.tohere, main = 'Ratios\n(for residents up to the given distance)', xlab='Distance from emissions source', ylab='Ratio')  
  points(x$dist, 100 * x$dshare.of.cases.tohere / x$pctd.tohere, col='blue')  
  legend('topright', legend = c('Demog group mean risk / everyone elses', 'Demog group share of cases / their share of pop'), fill = c('black','blue')) 
}
