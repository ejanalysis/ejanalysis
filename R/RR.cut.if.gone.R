#' @title How much is overal RR reduced if a given place did not exist?
#' @description As with RR function, calculates RR as ratio of means in one demographic group vs reference,
#'   based on Census demographic data and environmental indicator data on each place.
#'   Then this finds how much smaller RR would be if the given place did not exist.
#' @param e environmental indicator value
#' @param d demog group as fraction of pop
#' @param pop pop count
#' @param dref reference demog group as fraction
#' @param na.rm TRUE by default, should NA values be removed first
#'
#' @examples
#'  # x=RR.cut.if.gone(bg[ , names.e[8]], bg$pctlowinc, bg$pop)
#'  # summary(x*1000)
#'  mydat=data.frame(AQI=99:101, pctlowinc=c(0.20,0.30,0.40), pop=rep(1000,3))
#'  RR(mydat$AQI, mydat$pctlowinc, mydat$pop)
#'  RR.cut.if.gone(e=mydat$AQI, d=mydat$pctlowinc, pop=mydat$pop)
#' @export
RR.cut.if.gone <- function(e, d, pop, dref, na.rm=TRUE) {

#  contribution <- matrix()
#  dim(contribution) <- c(length(pop),
#                         ifelse(is.vector(e), length(e), length(e[1,])),
#                         ifelse(is.vector(d), length(d), length(d[1,])) )
#  dimnames(contribution) <- c('place', 'e', 'd')

  RR.cut.for.one.e <- function(e, d, pop, dref) {

    # for one envt factor,
    # for 1+ demog groups
    # BUT NOTE IF THIS REMOVES NA VALUES FOR one e factor and not for another, results used different places & people for different e factors ?
    # could NA values cause a problem here?

    if (is.vector(d)) {
      # for one e, one d:
        badrow <- is.na(rowSums(cbind(pop, e, d, dref),na.rm = FALSE) )
        x1=(pop * d * e) ; x1[badrow] <- 0
        x2=(pop * d) ; x2[badrow] <- 0
        x3=(pop * dref * e) ; x3[badrow] <- 0
        x4=(pop * dref); x4[badrow] <- 0
        a=sum(x1, na.rm = na.rm)
        b=sum(x2, na.rm = na.rm)
        c=sum(x3, na.rm = na.rm)
        d=sum(x4, na.rm = na.rm)

        rr= (a / b) / (c / d)

        rr.if.gone <- ((a - x1) / (b - x2) ) / ((c - x3) / (d - x4))

        contribution.1e <- rr - rr.if.gone
        contribution.1e
    } else {
      # for one e, multiple d: vectorized version

      # NOT DONE HERE YET CONVERTING TO  CONTRIBUTION ONLY
  # NEED TO HANDLE NA VALUES BUT NOT QUITE AS ABOVE

        (colSums(pop * d *      e, na.rm=na.rm) / colSums(pop * d,      na.rm=na.rm)) /
        (colSums(pop * (dref) * e, na.rm=na.rm) / colSums(pop * (dref), na.rm=na.rm))
    }
  }

  if (missing(e) || missing(d) || missing(pop)) { stop('Missing e, d, &/or pop argument')}
  if (any(d > 1, na.rm=TRUE)) {stop('d must be fractions < 1, not 0-100')}
  if (missing(dref)) {dref <- 1 - d} # if reference group's %s not specified, it is assumed to be everyone other than d

  # handle single envt factor
  if ( is.vector(e) && length(e) > 1 ) {
    return(RR.cut.for.one.e(e, d, pop, dref))
    # could warn if length(d)!=length(e) &  neither is an integer multiple of the other
    # same for length(pop)
  }

  # handle multiple envt factors
  if ((is.data.frame(e)  || is.matrix(e)) && length(e[ , 1]) > 1) {
    myresults <- sapply(e, function(x) RR.cut.for.one.e(x, d, pop, dref))
    return(myresults)
  }

  return(NA)
}
