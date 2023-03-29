#' create plot of 3 indicators versus distance
#' This makes a very simple scatter plot of 3 variables on y axis
#'   as a function of distance on the x axis.
#'   For example, it can show risk, percent demographic score, and population
#'   density, e.g., using numbers created at random by sim_riskbydistance()
#'
#' @param x data.frame such as output from [sim_riskbydistance()], with
#'   colnames like risk, pctd, popdensity, and dist (the defaults)
#'   or any 3 with values for y axis and 1 with values for x axis like distance.
#' @param xname colname in x so that x[,xname] can be used as x values in plot
#' @param xlab like in plot(), a string label for x axis
#' @param ynames 3 colnames in x, with 3 sets of y values for the scatter plot,
#'   but they have to be in the same units for the plot to make sense, so
#'   they could be percentages, or percentiles, for example.
#' @param mylegend 3 strings to use in legend(legend=mylegend, fill=mycolors)
#' @param mycolors 3 colors for legend
#' @export
#'
#' @seealso [sim_riskbydistance()]
plot_3_by_distance <- function(x, xname= 'dist', xlab="Distance (miles)",
                         ynames  =c('risk', 'pctd', 'popdensity'),
                         mylegend=c('Risk','% Demog','Pop density'),
                         mycolors=c('red','orange','blue')) {
  dat = x # to make the use of x and y less confusing here
  plot(x = dat$dist, xlim = c(0,50), xlab = 'Distance',
       y = dat$risk, ylim = c(0, max(dat$risk) * 1.05), ylab = '',
                                   col=mycolors[1])
  points(dat$dist, dat$pctd,       col=mycolors[2])
  points(dat$dist, dat$popdensity, col=mycolors[3])
  legend(x = 'topright', legend = mylegend, fill = mycolors)
}
