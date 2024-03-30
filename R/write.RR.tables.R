#' @title Save RR tables to disk, one per zone.
#'
#' @description This function breaks up a 3-dimensional table of RR values (e.g., by demographic group, by environmental indicator, by geographic zone),
#'   and saves data for each zone to a 2-dimensional table in a file.
#'   Requires table in format provided by [RR()] and related functions.
#'
#' @param my.RR.table Required RR table from function such as [RR()]
#' @param folder Optional directory, default is current working directory. Specifies where to save files.
#' @return Returns the file names as a character vector, after saving them locally.
#'   Saves one file per zone, with rownames and header.
#'   Format is one demographic group per row and one environmental indicator per column, plus max per row and max per column
#' @seealso [RR()]
#' @examples
#'   # RRS.REGION <- RR.table(mydat=bg, Enames=names.e,
#'   # Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='REGION')
#'   # write.RR.tables(RRS.REGION)
#' @export
write.RR.tables <- function(my.RR.table, folder=getwd()) {

  tabname <- deparse(substitute(my.RR.table))
  zonecount <- length(my.RR.table[1,1,])
  filename <- vector(NA, length=zonecount)

  for (i in 1:zonecount) {
    filename[i] <- file.path(folder, paste(tabname, '.', dimnames(my.RR.table)[[3]][i],'.csv' , sep=''))

    # You can't really sort cols or rows within each zone, unless just for purposes of saving each zone as a separate csv file,
    # because then that will make each zone have different sort order to its rows and cols, so they don't align across zones, which won't make sense for the 3-D array structure.

    sorted.zone <- my.RR.table[ , , i] # focus on just this zone
    sorted.zone   <- sorted.zone[order(sorted.zone[ , 1]), ] # reorder rows based on first column
    sorted.zone <- sorted.zone[ , order(sorted.zone[1, ])]  # reorder cols based on first row

    write.csv(sorted.zone, file=filename, row.names=TRUE)
  }

  return(filename)
}
