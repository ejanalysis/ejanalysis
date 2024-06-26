% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RR.cut.if.gone.R
\name{RR.cut.if.gone}
\alias{RR.cut.if.gone}
\title{How much is overal RR reduced if a given place did not exist?}
\usage{
RR.cut.if.gone(e, d, pop, dref, na.rm = TRUE)
}
\arguments{
\item{e}{environmental indicator value}

\item{d}{demog group as fraction of pop}

\item{pop}{pop count}

\item{dref}{reference demog group as fraction}

\item{na.rm}{TRUE by default, should NA values be removed first}
}
\description{
As with RR function, calculates RR as ratio of means in one demographic group vs reference,
based on Census demographic data and environmental indicator data on each place.
Then this finds how much smaller RR would be if the given place did not exist.
}
\examples{
 # x=RR.cut.if.gone(bg[ , names.e[8]], bg$pctlowinc, bg$pop)
 # summary(x*1000)
 mydat=data.frame(AQI=99:101, pctlowinc=c(0.20,0.30,0.40), pop=rep(1000,3))
 RR(mydat$AQI, mydat$pctlowinc, mydat$pop)
 RR.cut.if.gone(e=mydat$AQI, d=mydat$pctlowinc, pop=mydat$pop)
}
