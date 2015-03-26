RR.table.sort <- function(x, decreasing=TRUE) {
  # Takes one table, a 2-D slice of results of RR(), and sorts by col and by row to show highest RR values at top left
  # Example: RRS <- RR()
  x <- x[ order(x[ , 1], decreasing=TRUE), ]
  x <- x[ , order(x[1, ], decreasing=TRUE) ]
  return(x)  
}

