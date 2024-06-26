#' @title May be obsolete-not used -was Percentile and bin number fields, by zone, such as percentiles within each State
#'
#' @description May be obsolete- just uses make.bin.pctile.cols() which now has a zone parameter.
#'   Get weighted percentiles and bin numbers, by subset of rows (zone), for each column of data.
#'   This can be used to calculate population-weighted percentiles within each State, for national data on Census block groups, for example.
#' @param bg Data.frame of data field(s), weights (optional), and a unique ID (e.g., FIPS), one row per place if geographic data is used.
#' @param zone Optional vector specifying subsets of rows (e.g. State name) within which percentiles are calculated
#' @param wtsvarname Name of field in bg that has optional weights for weighted percentiles. Default is unweighted.
#' @param keyvarname Name of field in bg that has unique ID per row (per place). Default is "FIPS"
#' @param datavarnames Vector of names of fields in bg that have numeric data for which percentiles are calculated.
#' @param clean Does not use clean.fips() if FALSE, which helps if the countiesall or other list is not yet updated, for example and lacks some new FIPS code
#' @param ... Other parameters passed to [make.bin.pctile.cols()] such as `cutpoints` and `labels`
#' @return Returns a data.frame with one percentiles field and one bin numbers field for each of the datavarnames
make.bin.pctile.cols.byzone <- function(bg, zone, wtsvarname, keyvarname='FIPS', datavarnames=gsub(keyvarname, '', names(bg)), clean=TRUE,  ... ) {

  if (missing(wtsvarname)) {warning('wtsvarname such as pop or ACSTOTPOP was not specified, so returning unweighted percentiles instead of the usual population percentiles')}
  bg <- data.frame(bg, pop=1, stringsAsFactors = FALSE)

  # Error checking, e.g., make sure user did not include unique ID field (FIPS or whatever) in vector of data field names
  if (!missing(datavarnames)) {datavarnames=gsub(keyvarname, '', datavarnames)}
  if (!all(apply(bg[ , c(datavarnames, wtsvarname)], 2, class)=='numeric')) {stop('All of datavarnames and wtsvarname fields must be numeric')}

  # clean up FIPS/id if necessary
  if (keyvarname=='FIPS') {
    if (clean) {
      thefips <- ejanalysis::clean.fips(bg[ , keyvarname])
    } else {
      thefips <-     bg[ , keyvarname]
    }
    bg[ , keyvarname] <- thefips
  }

  if (anyDuplicated( bg[ , keyvarname])) {stop('Duplicate found in keyvarname field, but it must contain unique IDs')}

  return( ejanalysis::make.bin.pctile.cols(bg[ , datavarnames], weights=bg[ , wtsvarname], zone=zone, ...) )
}
