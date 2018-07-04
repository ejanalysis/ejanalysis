#' @title Write csv file lookup table - percentiles, mean, standard deviation
#' @description Given a data.frame, for each column in the data.frame,
#'   this function just returns percentiles, mean, and standard deviation.
#'   Also saves that as a csv file.
#' @param x Data.frame, required.
#' @param filename Name, or full path and name, of the file to be saved. Required.
#' @return A data.frame with percentiles, mean, and standard deviation. Same number of columns as x had.
#' @export
write.pctiles <- function(x, filename) {
  r <- data.frame(sapply(mydf, function(x) pctiles.exact(x)))
  r <- rbind(r, t(data.frame(mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
  r <- rbind(r, t(data.frame(std.dev = sapply(mydf, function(x) sd(x, na.rm = TRUE)))))
  r
  write.csv(r, file = paste(filename, ".csv", sep = ""))
  return(r)
}
