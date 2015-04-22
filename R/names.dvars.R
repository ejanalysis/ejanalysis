#' @name names.dvars
#' @docType data
#' @aliases names.dvars names.d names.d.subgroups Dlist demographic-variables
#' @title Fieldnames of demographic columns in ejanalysis data
#' @description This data set provides variables that hold the colnames
#'   of demographic fields in data.frames that may be used in the ejanalysis package
#'   to make it easier to refer to them as a vector, e.g., mydf[ , names.e]
#' @usage data('names.dvars')
#' @source Names developed for this package. No external data source.
#' @keywords datasets
#' @format A series of variables (each is a character vector of colnames):
#' \itemize{
#'  \item "names.d" (VSI.eo, VSI.svi6, pctmin, pctlowinc, pctlths, pctlingiso, pctunder5, pctover64)
#'  \item "names.d.bin"
#'  \item "names.d.eo"
#'  \item "names.d.eo.bin"
#'  \item "names.d.eo.pctile"
#'  \item "names.d.pctile"
#'  \item "names.d.subgroups"
#'  \item "names.d.subgroups.count"
#'  \item "names.d.subgroups.pct"
#'  \item "names.d.svi6"
#'  \item "names.d.svi6.bin"
#'  \item "names.d.svi6.pctile"
#'  #'  \item "Dlist" (this one is like names.d, but as a list, not a vector)
#' }
NULL
