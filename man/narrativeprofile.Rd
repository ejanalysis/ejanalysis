% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/narrativeprofile.R
\name{narrativeprofile}
\alias{narrativeprofile}
\title{Get County or Tract Narrative Profile webpage from Census}
\usage{
narrativeprofile(
  yearnumber = 2020,
  geotype = "county",
  statenumber2 = "24",
  countynumber3 = "031",
  tractnumber6 = NULL,
  launch = FALSE
)
}
\arguments{
\item{yearnumber}{for what year of the 1-year American Community Survey data}

\item{geotype}{county or tract}

\item{statenumber2}{two digit state FIPS (leading zero added if necessary)}

\item{countynumber3}{three digit county FIPS (leading zero added if necessary)}

\item{tractnumber6}{six digit tract FIPS}

\item{launch}{If set to TRUE, will launch a web browser and open the narrative profile page using the url}
}
\value{
URL that is link to Census Bureau webpage with narrative profile of multiple statistics
}
\description{
For 1 tract, URL could be for example
(https://www.census.gov/acs/www/data/data-tables-and-tools/narrative-profiles/
2019/report.php?geotype=tract&tract=700311&state=24&county=031)
For 1 county, URL could be for example
(https://www.census.gov/acs/www/data/data-tables-and-tools/narrative-profiles/
2019/report.php?geotype=county&state=21&county=017)
}
