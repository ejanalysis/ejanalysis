#' @title Write csv file lookup table - percentiles, mean, standard deviation
#'
#' @description Given a data.frame, for each column in the data.frame,
#'   this function just returns percentiles, mean, and standard deviation.
#'   Also saves that as a csv file.
#' @param x Data.frame, required.
#' @param filename Name, or full path and name, of the file to be saved. Required.
#' @return A data.frame with percentiles, mean, and standard deviation. Same number of columns as x had.
#' @export
write.pctiles <- function(x, filename) {

  write.wtd.pctiles.by.zone(x, filename = filename)
}
