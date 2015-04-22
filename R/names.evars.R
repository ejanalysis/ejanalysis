#' @name names.evars
#' @docType data
#' @aliases names.evars names.e environmental
#' @title Fieldnames of environmental indicator columns in ejanalysis data
#' @description This data set provides variables that hold the colnames
#'   of environmental indicator fields in data.frames that may be used in the ejanalysis package
#'   to make it easier to refer to them as a vector, e.g., mydf[ , names.e]
#' @usage data('names.evars')
#' @source Names developed for this package. No external data source.
#' @keywords datasets
#' @format A series of variables (each is a character vector of colnames):
#' \itemize{
#'  \item "names.e" (pm, o3, cancer, neuro, resp, dpm, pctpre1960, traffic.score, proximity.npl, proximity.rmp, proximity.tsdf, proximity.npdes)
#'  \item "names.e.bin"
#'  \item "names.e.pctile"
#' }
NULL
