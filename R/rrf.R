#' @title RR for one environmental indicator, one demographic group
#'
#' @description **Probably obsolete given [RR()]. \cr\cr
#'   Inputs are vectors over small places like Census blocks or block groups
#'   or possibly individuals (ideally) but then d would be a dummy=1 for selected group and 0 for people not in selected group
#'
#' @param e Environmental indicator vector, one number per place, required.
#'   e is environmental index or health risk level (e.g., PM2.5 concentration to which this person or place is exposed)
#' @param d Demographic indicator vector, required, one number per place, fraction of population that is in group of interest
#'   d is percent of place that is selected demog group (e.g. percent Hispanic) (or d=1 or 0 if this is a vector of individuals)
#' @param pop Population total per place, required, of which d is a fraction.
#'   pop is population count of place (or pop=1 if this is a vector of individuals)
#' @param na.rm Logical optional TRUE by default in which case NA values (missing values) are removed first. If FALSE, any NA value in pop, e, or d would make result NA.
#' @return Returns one number
#' @seealso [RR()]
#' @examples #
#'  # rrf(places$pm, places[ , unlist(Dlist)[1]],  places$pop)
#' @export
rrf <- function(e,d,pop, na.rm=TRUE) {
  return(
    (sum(pop*e* d,     na.rm=na.rm) / sum(pop* d,     na.rm=na.rm)) /
    (sum(pop*e* (1-d), na.rm=na.rm) / sum(pop* (1-d), na.rm=na.rm))
  )
}
