% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejRRadded.R
\name{ejRRadded}
\alias{ejRRadded}
\title{Formulas for local contributions to EJ metrics}
\usage{
ejRRadded(E, D = 1, P = 1, na.rm = TRUE, n = 5, ...)
}
\arguments{
\item{E}{Environmental indicator (e.g., risk, exposure, or any local indicator, but if it is individual risk then number of cases can be calculated rather than just people-points)}

\item{D}{Demographic group per place as fraction of P (default is 1)}

\item{P}{Population count per place (default is 1)}

\item{na.rm}{Default is TRUE.}

\item{n}{Default is 5.}

\item{...}{NOT USED. Would pass additional parameters to other functions}
}
\description{
Formulas for pop counts, pop demog pct, risk, cases, RR,
excess risk, excess cases (like EJ index sum),
and how those are different under 4 alt scenarios like E of D is set to that of dref.
}
