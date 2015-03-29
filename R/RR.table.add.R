#'@title Merge table of Relative Risk results for some zones with table for USA overall
#'@description xxx.
#'@details xxx.
#'Returns a xxx.
#'xxx.
#'
#'@param xxx xxx/what/type/default.
#'@param xxx xxx/what/type/default.
#'@param xxx xxx/what/type/default.
#'@param xxx xxx/what/type/default.
#'@return Returns a xxx/type/what/size.
#' @template seealsoRR
#' @keywords EJ
#' @examples
#' RRS.US  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop')
#' RRS.ST  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='ST')
#' RRS <- RR.table.add(RRS.ST, RRS.US)
#' RRS[ 'pctlowinc', , ]
#' RRS[ , , 'CA']
#' RRS[ , 'pm', ]
#' RRS.REGION  <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='REGION')
#' RRS2 <- RR.table.add(RRS, RRS.REGION)
#' RRS2[ , , '8']
#' @note Future work: xxx.
#' @export
table.add <- function(rrs1, rrs2, zones2) {

  ############################
  # MERGE ANY TABLE OF ZONES AND US TABLE *****
  ############################
  # Function to merge set of Relative Risk tables as 3D array from RR.table() with one or more such tables
  # e.g., for the overall area (aggregate of those places), also from RR.table()

  zones2 <- dimnames(rrs2)[[3]]
  len1 <- dim(rrs1)[3]
  len2 <- dim(rrs2)[3]
  RRS <- array(dim=c(dim(rrs1)[1], dim(rrs1)[2], len1 + len2 ) )
  RRS[ , , 1:len1 ]   <- rrs1
  RRS[ , , (1 + len1):(len1 + len2) ] <- rrs2
  dimnames(RRS) <- list( dimnames(rrs1)[[1]], dimnames(rrs1)[[2]], c(dimnames(rrs1)[[3]], zones2) )
  return(RRS)
}
