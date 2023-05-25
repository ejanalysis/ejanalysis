#' @title flagged.by
#' @description Flag which cells are at or above some threshold.
#' @details For a matrix with a few cols of related data, find which cells are at/above (or below) some threshold.
#' Returns a logical matrix, with TRUE for each cell that is at/above the threshold.
#' Can be used in EJ analysis as 1st step in identifying places (rows) where some indicator(s) is/are at/above a threshold, threshold value.
#'
#' @param x Data.frame or matrix of numbers to be compared to threshold value.
#' @param threshold Numeric. The threshold or threshold to which numbers are compared. Default is arithmetic mean of row. Usually one number, but can be a vector of same length as number of rows, in which case each row can use a different threshold.
#' @param or.tied Logical. Default is FALSE, which means we check if number in x is greater than the threshold (>). If TRUE, check if greater than or equal (>=).
#' @param below Logical. Default is FALSE. If TRUE, uses > or >= threshold. If FALSE, uses < or <= threshold.
#' @param ... optional additional parameters to pass to [analyze.stuff::cols.above.which()]
#' @return Returns a logical matrix the same size as x.
#' @seealso cols.above.which,  another name for the exact same function.
#' @seealso cols.above.count or cols.above.pct to see, for each row, count or fraction of columns with numbers at/above/below threshold.
#' @seealso flagged.only.by to find cells that are the only one in the row that is at/above/below the threshold.
#' @seealso rows.above.count, rows.above.pct, rows.above.which
#' @examples
#' out <- flagged.by(x<-data.frame(a=1:10, b=rep(7,10), c=7:16), threshold=7)
#' x; out # default is or.tied=FALSE
#' out <- flagged.by(data.frame(a=1:10, b=rep(7,10), c=7:16), threshold=7, or.tied=TRUE, below=TRUE)
#' out
#' out <- flagged.by(data.frame(a=1:10, b=rep(7,10), c=7:16) )
#' # Compares each number in each row to the row's mean.
#' out
#' @note Future work: these functions could have wts, na.rm, & allow cutpoints or benchmarks as a vector (not just 1 number), & have benchnames.
#' @export
flagged.by <- function(x, threshold, or.tied, below, ...) {
  analyze.stuff::cols.above.which(x=x, cutoff=threshold, or.tied=or.tied, ...)
}
