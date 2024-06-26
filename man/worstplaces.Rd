% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/worstplaces.R
\name{worstplaces}
\alias{worstplaces}
\title{Places with worst RR}
\usage{
worstplaces(rrs, d = "pctlowinc", e = "pm", n = 10, digits = 2)
}
\arguments{
\item{rrs}{Required. This has to be the output of one of the functions like \code{\link[=RR.table]{RR.table()}}}

\item{d}{name of demographic field with percent (fraction) of population that is in each given demographic group, in dimnames(rrs)[\link{1}]}

\item{e}{name of environmental risk factor in dimnames(rrs)[\link{2}]}

\item{n}{worst 10 by default}

\item{digits}{round to 2 by default}
}
\value{
matrix
}
\description{
View one key part of table of Relative Risk results by group by envt risk factor
}
\examples{
 \dontrun{
  data(bgtest, package = 'ejanalysis')
  RRS.US  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop')
  RRS.ST  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = names.d, popcolname = 'pop',
                  Zcolname = 'ST')
  RRS <- RR.table.add(RRS.ST, RRS.US)
  worstplaces(RRS.US)
}
}
\seealso{
\code{\link[=RR]{RR()}}
}
