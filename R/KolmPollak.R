#' @title Kolm-Pollak Inequality Index adjustable for indexing inequality in bad outcomes
#' @description This is a simple wrapper for the ineq::Kolm function, but
#'   it makes explicit the need to transform x first if x measures a harm or cost.
#' @details
#'   The Kolm-Pollak inequality index is provided by the package ineq
#'   but that index is only suitable for use with a variable where larger values are preferred, such as with income.
#'   The need for this type of transformation is noted by Maguire and Sheriff (2011) in <http://dx.doi.org/10.3390/ijerph8051707>
#'   Many variables represent negative outcomes, such as measures of risks to health, or costs.
#'   For those types of variables, the variable should first be transformed by multiplying it by negative one.
#'   This function does that when a parameter is set appropriately. \cr \cr
#'   The typical KP index says inequality is **worse if some incomes are very low**
#'   (than if some are very high). The adjusted KP index says
#'   inequality is **worse if some costs or health risks are very high**
#'   (than if some are very low).
#' @param x Same as in [ineq::Kolm()].
#' @param parameter Same as in [ineq::Kolm()].
#' @param na.rm Same as in [ineq::Kolm()].
#' @param bigbadx Optional, default is FALSE. Setting bigbadx = TRUE means bigger x values are bad, as in the case of x measuring levels of risk to health, or exposure to a pollutant.
#'   If bigbadx=TRUE, the x values are first transformed into 0 - x
#' @return This returns what Kolm does, except if bigbadx=TRUE it first transforms x.
#' @examples
#'  # Typical KP index says
#'  #   inequality is worse
#'  #   if some incomes are very low
#'  #   (than if some are very high):
#'  KolmPollak(c(-5,0,1), bigbadx = FALSE) #[1] 2.577229
#'  KolmPollak(c(-1,0,5), bigbadx = FALSE) #[1] 1.549793
#'
#'  # Adjusted KP index says
#'  #   inequality is worse
#'  #   if some costs or health risks are very high
#'  #   (than if some are very low):
#'  KolmPollak(c(-1,0,5), bigbadx = TRUE) #[1] 2.577229
#'  KolmPollak(c(-5,0,1), bigbadx = TRUE) #[1] 1.549793
#'
#'  set.seed(99)
#'  x=rep(c(1,2,5),50)*rnorm(mean=1, sd=0.2, n=50*3)
#'  ineq::Kolm(x)
#'  KolmPollak(x)
#'  KolmPollak(x, bigbadx = TRUE)
#'
#' @export
KolmPollak <- function(x, parameter= 1, na.rm=TRUE, bigbadx=FALSE) {

  if (bigbadx) {x <- 0 - x}
  return(ineq::Kolm(x = x, parameter = parameter, na.rm = na.rm))
}
