#' Simulate disparities as risk, pop density, and demographics vary by distance
#'
#'
#' Creates a simple set of 100 distances and at each distance simulated
#'   demographics, risk levels, population density and size, etc.
#'   Useful to explore how those decay with distance and how the
#'   EJ metrics one might use depend on those factors and domain analyzed.
#'
#' @param dist Best not to change this. Distances simulated, such as 1 through 100 (that could be kilometers, for example).
#' @param ypopdensity.start population density at closest point to emissions source
#' @param ypopdensity.end at furthest point
#' @param ypopdensity.rate decay rate with distance such as 0.95 for 5 percent drop per 1 of the 100 distance increments
#' @param ydemog.start Demographic group of interest nearby, as percent of pop, 0-100
#' @param ydemog.end  at furthest point
#' @param ydemog.rate decay rate with distance such as 0.95 for 5 percent drop per 1 of the 100 distance increments
#' @param yrisk.start Risk metric (e.g., 0 to 100) at nearest point.
#' @param yrisk.end  at furthest point
#' @param yrisk.rate decay rate with distance such as 0.95 for 5 percent drop per 1 of the 100 distance increments
#' @param silent whether to print subset of table to console, and suggested plot code
#'
#' @return silently returns data.frame of many indicators, at each of the 100 distances
#' @export
#'
#' @examples
#'   x <- sim_riskbydistance()
#'   sim_plotrisk(x)
#'   sim_plotratios(x)
sim_riskbydistance <- function(dist = 1:100,
                               ypopdensity.start = 100,
                               ypopdensity.end = 50,
                               ypopdensity.rate = 0.95,
                               ydemog.start = 66,
                               ydemog.end = 33,
                               ydemog.rate = 0.95,
                               yrisk.start = 100,
                               yrisk.end = 1,
                               yrisk.rate = 0.80,
                               silent = FALSE) {
  dist = 1:100 # distance in miles from source of risk
  popdensity = (ypopdensity.end + (ypopdensity.start - ypopdensity.end) * (ypopdensity.rate ^
                                                                             (0:99)))
  pop.tohere = round(pi * (dist ^ 2) * popdensity * 10, 0)
  popslice = pop.tohere - c(0, pop.tohere[1:99])

  pctd = ydemog.end + (ydemog.start - ydemog.end) * (ydemog.rate ^ (0:99)) # 100*(0.99)^(0:99)
  pctd.tohere = cumsum(pctd * popslice) / cumsum(popslice)

  risk = yrisk.end + (yrisk.start - yrisk.end) * (yrisk.rate ^ (0:99))
  # risk.d.slice = risk
  # risk.nond.slice = risk

  risk.tohere      = cumsum(risk              * popslice) / pop.tohere  #risk.tohere = # weighted.mean(risk, popslice)
  risk.d.tohere    = cumsum(risk * (pctd / 100)       * popslice) / cumsum((pctd /
                                                                              100)       * popslice) # weighted.mean(risk, popslice * pctd)
  risk.nond.tohere = cumsum(risk * (1 - pctd / 100) * popslice) / cumsum((1 - pctd /
                                                                            100) * popslice) # weighted.mean(risk, popslice * (1 - pctd))
  riskratio.tohere = risk.d.tohere / risk.nond.tohere

  cases     = risk * popslice
  casesd    = risk * popslice *         pctd / 100
  casesnond = risk * popslice * (1 - pctd / 100)

  cases.tohere = cumsum(cases)
  cases.d.tohere = cumsum(casesd)
  cases.nond.tohere = cumsum(casesnond)

  caseratio.tohere = casesd / casesnond
  # dshare.of.cases.slice = casesd / cases
  dshare.of.cases.tohere = cases.d.tohere / cases.tohere

  outputs = list(
    dist = dist,
    risk = risk,
    pctd = pctd,
    pctd.tohere = pctd.tohere,
    # ydemog.start=ydemog.start,
    # ydemog.end=ydemog.end,
    popdensity = popdensity,
    popslice = popslice,
    pop.tohere = pop.tohere,

    risk.tohere = risk.tohere,
    risk.d.tohere = risk.d.tohere,
    risk.nond.tohere = risk.nond.tohere,
    riskratio.tohere = riskratio.tohere,
    caseratio.tohere = caseratio.tohere,

    cases = cases,
    casesd = casesd,
    casesnond = casesnond,
    cases.tohere = cases.tohere,
    cases.d.tohere = cases.d.tohere,
    cases.nond.tohere = cases.nond.tohere,
    # dshare.of.cases.slice = dshare.of.cases.slice,
    dshare.of.cases.tohere = dshare.of.cases.tohere
  )

  selectedcolumns <-
    c(
      'dist',
      'popdensity',
      'pop.tohere',
      'risk',
      'risk.tohere',
      'risk.d.tohere',
      'risk.nond.tohere',
      'riskratio.tohere',
      'pctd.tohere',
      'dshare.of.cases.tohere',
      'cases.d.tohere',
      'cases.nond.tohere'
    )
  if (!silent) {
    print(as.data.frame(sapply(outputs, signif, 3))[c(1, (1:10) * 10), selectedcolumns])
    cat("\nTry these: \n\n ")
    cat("x <- sim_riskbydistance() \n\n")
    # cat("plot(x$dist, x$riskratio.tohere, main = 'Ratio of mean risk among Demog group to mean risk among everyone else\n(for residents up to the given distance)', xlab='Distance from emissions source', ylab='Risk Ratio') \n\n")
    cat("sim_plotrisk(x)  \n\n")
    cat("sim_plotratios(x)  \n\n")
    cat("sim_plotratios()  \n\n")
  }

  # cat("plot(x$dist, x$riskratio.tohere, main = 'Ratios of mean risk or share of pop risk among Demog group\n to risk among everyone else or to share of population (for residents up to the given distance)', xlab='Distance from emissions source', ylab='Ratio')  \n ")
  # cat("points(x$dist, 100 * x$dshare.of.cases.tohere / x$pctd.tohere, col='blue')  \n ")
  # cat("legend('topright', legend = c('Demog group mean risk / everyone elses', 'Demog group share of cases / their share of pop'), fill = c('black','blue')) \n\n ")

  sim_plotrisk(outputs)

  invisible(as.data.frame(outputs))

}
