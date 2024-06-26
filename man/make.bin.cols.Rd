% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.bin.cols.R
\name{make.bin.cols}
\alias{make.bin.cols}
\title{Create a Bin Numbers Column for each Percentiles Column}
\usage{
make.bin.cols(
  pctile.df,
  as.df = TRUE,
  cutpoints = c((0:9)/10, 0.95, 1),
  labels = 1:11,
  prefix = "bin."
)
}
\arguments{
\item{pctile.df}{Data.frame (or matrix or vector), required. Typically percentiles as decimal fractions, 0 to 1, one per place.}

\item{as.df}{Logical value, optional, TRUE by default. Defines whether results are data.frame or matrix.}

\item{cutpoints}{Optional vector of cutpoints defining edges of bins. Default is every 0.10 from 0 through 1.00, as well as 0.95}

\item{labels}{Vector of bin numbers (defining what is returned for values in those bins), optional (default is 1 through 11, and NA values are put in bin 0).}

\item{prefix}{Optional character element, ".bin" by default, pasted as prefix to each column name of pctile.df, and used as names of returned columns.}
}
\value{
Returns a data.frame, matrix, or vector (depending on \code{as.df}) the same shape as pctile.df
}
\description{
Simplifies creation of data.frame or matrix (or vector) with columns that specify bins by bin number,
based on values (percentiles for example) and specified cutpoints. Each bin is defined by cutpoints.
}
\details{
This is one way to prepare data to be mapped in a series of choropleths, for example (color-coded maps).

The default bins 0-11 are defined as follows: \cr
bin 0: PCTILE=NA \cr
... \cr
bin 9:  0.80<=PCTILE<0.90 \cr
bin 10: 0.90<=PCTILE<0.95 \cr
bin 11: 0.95<=PCTILE<=1.00  \cr
}
\examples{
	#  new.bin.cols <- make.bin.cols(places[ , names.e.pctile])
	# new.bin.cols <- make.bin.cols( new.pctile.cols )
}
\seealso{
\code{\link[=make.bin.pctile.cols]{make.bin.pctile.cols()}} and \code{\link[=assign.pctiles]{assign.pctiles()}}
}
