% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RR.table.view.R
\name{RR.table.view}
\alias{RR.table.view}
\title{Just print Relative Risks (RR) info by demog group by risk type by zone}
\usage{
RR.table.view(
  x,
  d = dimnames(x)[[1]],
  e = dimnames(x)[[2]],
  zone = dimnames(x)[[3]]
)
}
\arguments{
\item{x}{A three dimensional array that is created by \code{\link[=RR]{RR()}} and has format like RRS\link{d, e, zone}}

\item{d}{Optional. Vector of names of demographic groups that must be subset of first dimension of RRS. Default is all.}

\item{e}{Optional. Vector of names of environmental risk indicators that must be subset of second dimension of RRS. Default is all.}

\item{zone}{Optional. Vector of zone names that must be subset of names of third dimension of RRS. Default is all.}
}
\value{
prints RRS\link{d, e, zone}
}
\description{
Currently does not do anything other than print. Takes zone-specific RRS values and prints them for viewing.
Just another way to specify which subset of the RR array you want to view.
}
\examples{
 t(round(RR.table.view(
   RR.table(bgtest, names.e[1:3], names.d[3:4], 'pop', Zcolname = 'statename'),
   d = 'pctlowinc'
  ), 2))
}
\seealso{
\code{\link[=RR]{RR()}}
}
