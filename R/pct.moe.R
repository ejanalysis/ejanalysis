#' @title Margin of Error for a Percent (Ratio)
#'
#' @description
#'   Estimates the margin of error (MOE) that characterizes uncertainty, for a ratio (a/b),
#'   based on a, b, and their MOE values.
#'
#' @details
#'   This is based on US Census Bureau recommendations for working with American Community Survey summary file data.
#'   See \url{http://www.census.gov/programs-surveys/acs/guidance.html} and \url{http://www.census.gov/library/publications/2009/acs/researchers.html}
#'   For example, one can estimate MOE for percent poor in a block group, given
#'   estimates and margins of error for the count who are poor, and the count for whom poverty status is known.
#'
#' @param a Numerators, as a vector
#' @param b Denominators, as a vector (should be same length as a, or it is recycled)
#' @param a.moe Margins of error for numerators, as a vector  (should be same length as a).
#' @param b.moe Margins of error for denominators, as a vector (should be same length as a, or it is recycled).
#' @return Returns margin(s) of error same shape as parameter a
#' @seealso \code{\link{sum.moe}}
#' @examples
#' 	 x <- pct.moe(15, 100, 3, 10)
#' @export
pct.moe <- function(a, b, a.moe, b.moe) {
  ratio = ifelse(b>0, a / b, 0)
  dif <- a.moe^2 - ((ratio)^2 * b.moe^2)
  undersign <- ifelse(dif >0, dif, a.moe^2 + ((ratio)^2 * b.moe^2))
  ifelse(b>0, sqrt( undersign )/ b, 0)
}
