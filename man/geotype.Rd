% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geotype.R
\name{geotype}
\alias{geotype}
\title{Infer Type of Data as FIPS / ANSI Codes or Names of U.S. Geographies - obsolete.
See EJAM package function called fipstype()}
\usage{
geotype(x, cleancounties = TRUE)
}
\arguments{
\item{x}{Required vector of one or more numeric or character FIPS and/or names of geographic locations.
Allowed types are State, County (or equivalent), tract, block group, and block.
Names for tracts, blockgroups, and blocks are not provided or interpreted.
FIPS codes here are all the relevant digits starting with the 2-character state FIPS,
so county fips must be 4-5 digits or characters for example (leading zeroes are inferred where
possible and included in outputs). See \code{\link[=clean.fips]{clean.fips()}} for details.}

\item{cleancounties}{whether to try to validate county FIPS based on list that ideally is always up to date}
}
\value{
***TBD *** Returns *** types as character strings, and maybe cleaned values themselves? ***
}
\description{
Tries to interpret vector of one or more FIPS codes and/or names of geographies.
}
\examples{
# none yet
}
\seealso{
\code{\link[=geofips]{geofips()}}, \code{\link[=get.fips.st]{get.fips.st()}} and related functions noted there, \code{\link[=clean.fips]{clean.fips()}}, \code{\link[=get.state.info]{get.state.info()}}
}
