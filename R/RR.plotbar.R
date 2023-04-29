#' @title Draw barplot comparing mean score in a group vs reference group
#'
#' @description
#'  Draws a plot using group means from [RR.means()]
#' @param x Results of [RR.means()] function, mean indicator score or risk by demographic group
#' @param dname Demographic group name to be found among names of first dim of x. Required.
#' @param ename Environmental factor or risk type name to be found as names of second dim of x. Required.
#' @param dlab optional character vector of label to use instead of dname on the plot. Default is dname.
#' @param elab optional character vector of label to use instead of ename on the plot. Default is ename.
#' @param reflab optional character vector of lable to use for reference group. Default is Non-dlab (e.g., "Non-Poor")
#' @param cex.names optional numeric vector passed to [barplot()]
#' @param cex.axis optional numeric vector passed to [barplot()]
#' @param ... optional other parameters passed to [barplot()]
#' @return draws a barplot, returns the two values as named list
#' @seealso [RR.plot()]
#' @examples \dontrun{
#'     data(bgtest, package = 'ejanalysis')
#'     ### x should be the output of RR.means()
#'     x <- RR.means(e = bgtest$traffic.score, d = bgtest$pctmin, pop = bgtest$pop)
#'     RR.plotbar(x, 'pctlths', 'proximity.rmp')
#'     RR.plotbar(x, 'pctmin', 'proximity.tsdf', dlab = 'Minorities',
#'       elab = 'Haz. Waste TSD Facility Proximity Score', ylab = 'Proximity Score')
#'  }
#' @export
RR.plotbar <- function(x, dname, ename, dlab = dname, elab = ename,
                       reflab = paste('Non-', dlab, sep = ''), cex.names = 1.9, cex.axis = 1.5, ...) {

  barplot(
    x[ dname, 1:2, ename],
    names.arg = c(dlab, reflab),
    main = paste('Mean', elab, 'in Each Group'),
    cex.axis = cex.axis,
    cex.names = cex.names,
    ...
  )
  out <- x[dname, 1:2, ename]
  names(out) <- c(dlab, reflab)
  return(out)
}
