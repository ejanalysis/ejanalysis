#' @title Draw lineplot comparing RR values by group
#'
#' @description
#'  Draws a plot using relative risk information, one line per demographic group.
#' @param rrs Table as from \code{\link{RRS}} function, of relative risk by demographic group, risk type, and zone (3 dimensions).
#' @param dnames Demographic group names to be found as names of first dim of RRS. Optional, default is \code{\link[ejscreen]{names.d.subgroups}} and \code{\link[ejscreen]{names.d}} from the ejscreen dataset.
#' @param enames Environmental factor or risk type names to be found as names of second dim of RRS. Optional, default is \code{\link[ejscreen]{names.e}} from ejscreen package.
#' @param zone Zone name to be found among names for third dim of RRS. Default is "USA"
#' @param enames.nice optional character vector of labels to use instead of enames on the plot, same length and order as enames
#' @param sorted optional logical, default is TRUE. Whether to sort before plotting.
#' @param margins optional numeric vector, default is c(1, 2, 3). Specifies which dimensions to sort if sorted is TRUE.
#' @param decreasing optional logical, default is TRUE. How to sort if sorted is TRUE. All sorted dimensions get sorted the same way.
#' @param type optional as in \code{\link{plot}}, default is 'b' for both lines and points
#' @param mycolors optional vector of colors to use for the vector of dnames, default is from \code{\link{rainbow}}
#' @param maxchar optional number, default 15. x axis labels are truncated to this length.
#' @param cex.axis optional default 0.5, size of enames lables on x axis
#' @param cex.legend optional default 0.8, size of dnames in legend
#' @param ... optional additional parameters to pass to \code{\link{plot}}
#' @return draws a line plot
#' @seealso \code{\link{RR.table}}
#' @examples #
#'   ## #
#' @export
RR.plot <-
  function(rrs,
           dnames,
           enames,
           zone = 'USA',
           enames.nice,
           mycolors,
           type = 'b',
           maxchar = 15,
           cex.axis = 0.5,
           cex.legend = 0.8,
           sorted = TRUE,
           margins = c(1, 2, 3),
           decreasing = TRUE,
           ...) {
    RRS <- rrs
    if (missing(dnames)) {
      try(data(names.dvars, names.e.nice, package = 'ejscreen'))
      if (!exists('names.e.nice')) {
        stop('need to specify dnames and enames.nice if ejscreen package not installed')
      }
      dnames <- c(names.d, names.d.subgroups)
    }
    if (any(!(dnames %in% dimnames(RRS)[[1]]))) {
      stop('all dnames must be in 1st dim of RRS')
    }
    if (missing(enames)) {
      enames <- names.e
    }
    if (any(!(enames %in% dimnames(RRS)[[2]]))) {
      stop('all enames must be in 2d dim of RRS')
    }
    if (missing(enames.nice)) {
      enames.nice <- enames
      # for the enames they specified that are identical to std names.e, substitute the nice version from the ejscreen pkg.
      enames.nice[enames %in% names.e] <-
        names.e.nice[match(enames[enames %in% names.e], names.e)]
    }
    if (missing(mycolors)) {
      mycolors <- rainbow(length(dnames))
    }

    if (sorted) {
      RRS <- RR.table.sort(RRS, margins = margins, decreasing = decreasing)
    }

    # do the first D to create the plot
    i <- 1
    plot(
      RRS[dnames[i], enames, zone],
      col = mycolors[i],
      main = paste('RR by group for ', zone, ' for ', 'several risk indicators', sep = ''),
      ylab = 'Ratio of given group mean to reference group mean',
      xlab = 'Environmental Indicators or Risk Indicators',
      xaxt = 'n',
      type = type,
      ylim = c(min(RRS[ , enames, zone], na.rm = TRUE), max(RRS[ , enames, zone], na.rm = TRUE)),
      ...
    )

    abline(h = 1, col = 'gray')
    axis(
      side = 1,
      at = 1:length(enames.nice),
      labels = substr(enames.nice, 1, maxchar),
      cex.axis = cex.axis
    )
    legend(
      x = 'topleft', legend = dnames,
      col =  mycolors, lty = 1,
      cex = cex.legend,
      ncol = 2,
      inset = 0.05,
      bty = 'n'
      )
    #    could also include in legend (after the D name) the name of the E with the max RR for that D, or the max RR value
    # legend = paste(
    #   dlabs,
    #   ' (mean=',
    #   signif(wtdmeans, 4),
    #   ', ',
    #   round(wtdmeans / wtdmeans[dlabs == 'overall'], 2),
    #   'x overall mean)',
    #   sep = ''
    # ),

    # add the rest of the Ds to the plot as one line each
    for (i in 2:length(dnames)) {
      lines(RRS[dnames[i], enames, zone],
            type = type,
            col = mycolors[i]
            )
    }
  }
