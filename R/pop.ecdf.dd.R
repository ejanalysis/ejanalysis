#' @title Draw an Ecdf plot showing the distribution of scores in each demographic subgroup
#'
#' @description
#'  Draws a plot using \code{\link[Hmisc]{Ecdf}}, overlaying cumulative distribution functions,
#'  one for each subgroup specified, and one for overall population.
#'  Useful to compare subgroups based on the distribution of scores among people in that subgroup
#'  using data from small places like census block groups, based on having for each place the pop total
#'  and % of pop that is in each group.
#'
#' @param e required numeric vector of scores (e.g., environmental indicator values) for places like block groups, one place per row
#' @param elab required character single element vector with text label for environmental score, used in xlab of plot
#' @param mydf required data.frame where each row is a place and each column is the percent of place population that is in given demographic subgroup, one subgroup per column
#' @param dlabs optional, charater vector that specifies text for legend, same length as number of columns in mydf
#' @param mywt optional, default is unweighted, but normally would specify as the population counts for places analyzed
#' @param main optional title used at top of plot
#' @param mycolors optional vector of colors as long as number of columns in mydf, specifies colors of lines
#' @param refcolor optional color, default is 'black'
#' @param sorted optional logical, default is TRUE. Should the legend be sorted in order of population weighted mean values of e?
#' @param main Optional character specifying plot title. Default title notes names of subgroups
#' @param ... other optional parameters to pass to Ecdf
#' @return draws a plot
#' @seealso \code{\link[Hmisc]{Ecdf}} \code{\link{RR}} \code{\link{pop.cdf}}   \code{\link{pop.cdf2}} \code{\link{pop.ecdf}}  \code{\link{pop.cdf.density}}
#' @examples
#' ###
#' \dontrun{
#' set.seed(99)
#' pctminsim=c(runif(7000,0,1), pmin(rlnorm(5000, meanlog=log(0.30), sdlog=1.7), 4)/4)
#' popsim= runif(12000, 500, 3000)
#' esim= rlnorm(12000, log(10), log(1.15)) + rnorm(12000, 1, 0.5) * pctminsim - 1
#' }
#' @export
pop.ecdf.dd <- function(e, elab, mydf, dlabs = colnames(mydf), mywt = rep(1, NROW(mydf)), main, mycolors, refcolor = 'black', sorted = TRUE, log = '', ...) {

  if (missing(elab)) {elab <- deparse(substitute(e))}
  if (missing(main)) {
    # mymain <- paste('Scores in overall population vs among\n', paste(dlabs, collapse = ', '), '')
    mymain <- 'Scores within overall population vs \nwithin a given subgroup'
  } else {
    mymain <- main
  }

  mye <- e; myelab <- elab
  refd <- rep(1, NROW(mydf))

  # Append overall ref group to subgroups
  #  cols of mydf incl refd
  #  dlabs incl refd
  # mycolors incl refd because calc after refd in mydf unless user specified
  #  wtdmeans incl refd because calculated after refd in mydf
  mydf <- data.frame(mydf, overall = refd, stringsAsFactors = FALSE)
  dlabs <- c(dlabs, 'overall')
  # wtdmeans <- sapply(mydf, function(x) weighted.mean(mye, x * mywt))
  wtdmeans <- sapply(mydf, function(x) Hmisc::wtd.mean(mye, weights = x * mywt))

  if (missing(mycolors)) {
    n <- NCOL(mydf) - 1 # all but ref group
    if (n <= 12) {
      mycolors <- c(RColorBrewer::brewer.pal(n, "Paired"), refcolor)
    } else {
      mycolors <- c(rainbow(n), refcolor)
    }
  } else {
    # user specified colors prob are missing one for ref group
    if (NCOL(mydf) == length(mycolors) ) {
      warning('using mycolors for all including overall pop so refcolor ignored')
      mycolors <- mycolors
    } else {
      # user seems to have specified right number?
      mycolors <- c(mycolors, refcolor)
    }
  }

  if (sorted) {
    # sort wtdmeans
    # sort columns of mydf
    # sort dlabs
    # sort mycolors?? no need
    ordering <- order(wtdmeans, decreasing = TRUE)
    wtdmeans <- wtdmeans[ordering]
    mydf <- mydf[ , ordering]
    dlabs <- dlabs[ordering]
    mycolors <- mycolors[ordering]
  }

  myxlim <- c(min(mye, na.rm = TRUE), max(mye, na.rm = TRUE))
  if ('x' %in% log) {
    where <- ifelse(log10(wtdmeans[dlabs == 'overall']) > (log10(myxlim[1]) + ((log10(myxlim[2]) - log10(myxlim[1])) / 4)), 'topleft', 'bottomright')
  } else {
    where <- ifelse(wtdmeans[dlabs == 'overall'] > (myxlim[1] + ((myxlim[2] - myxlim[1]) / 4)), 'topleft', 'bottomright')
  }

  for (i in 1:NCOL(mydf)) {

    myd <- mydf[ , i] #; mydlab <- dlabs[i]

    if (i == 1) {
      pop.ecdf(mye, myd, mywt,
               add = FALSE,
               allothers = FALSE,
               xlab = myelab, xlim = myxlim,
               main = mymain,
               col = mycolors[i], log = log, ...)

      legend(x = where, legend = paste(dlabs, ' (mean=', signif(wtdmeans, 4), ', ', round(wtdmeans/wtdmeans[dlabs == 'overall'], 2), 'x overall mean)', sep = ''), col = mycolors, lty = 1, bty = 'n')

    } else {
      pop.ecdf(mye, myd, mywt,
               add = TRUE,
               allothers = FALSE,
               col = mycolors[i], log = log, ...
      )
    }
    abline(v = wtdmeans[i], col = mycolors[i])
  }
}
