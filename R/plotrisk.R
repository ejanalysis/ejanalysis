#' create plot of basic info from sim_riskbydistance output
#'
#' @param x output from sim_riskbydistance
#'
#' @export
#'
#' @seealso sim_riskbydistance
sim_plotrisk <- function(x) {
  plot(x$dist, x$risk, xlab = 'Distance', ylab = '', col='red', ylim = c(0, max(x$risk)*1.05),xlim = c(0,50))
  points(x$dist, x$pctd, col='orange')
  points(x$dist, x$popdensity, col='blue')
  legend(x = 'topright', legend = c('Risk','% Demog', 'Pop density'), fill = c('red','orange','blue'))
}
