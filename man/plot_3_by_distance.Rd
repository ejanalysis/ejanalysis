% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_3_by_distance.R
\name{plot_3_by_distance}
\alias{plot_3_by_distance}
\title{create plot of 3 indicators versus distance
This makes a very simple scatter plot of 3 variables on y axis
as a function of distance on the x axis.
For example, it can show risk, percent demographic score, and population
density, e.g., using numbers created at random by sim_riskbydistance()}
\usage{
plot_3_by_distance(
  x,
  xname = "dist",
  xlab = "Distance (miles)",
  ynames = c("risk", "pctd", "popdensity"),
  mylegend = c("Risk", "\% Demog", "Pop density"),
  mycolors = c("red", "orange", "blue")
)
}
\arguments{
\item{x}{data.frame such as output from \code{\link[=sim_riskbydistance]{sim_riskbydistance()}}, with
colnames like risk, pctd, popdensity, and dist (the defaults)
or any 3 with values for y axis and 1 with values for x axis like distance.}

\item{xname}{colname in x so that x\link{,xname} can be used as x values in plot}

\item{xlab}{like in plot(), a string label for x axis}

\item{ynames}{3 colnames in x, with 3 sets of y values for the scatter plot,
but they have to be in the same units for the plot to make sense, so
they could be percentages, or percentiles, for example.}

\item{mylegend}{3 strings to use in legend(legend=mylegend, fill=mycolors)}

\item{mycolors}{3 colors for legend}
}
\description{
create plot of 3 indicators versus distance
This makes a very simple scatter plot of 3 variables on y axis
as a function of distance on the x axis.
For example, it can show risk, percent demographic score, and population
density, e.g., using numbers created at random by sim_riskbydistance()
}
\seealso{
\code{\link[=sim_riskbydistance]{sim_riskbydistance()}}
}
