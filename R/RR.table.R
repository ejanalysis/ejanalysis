#' @title Table of Relative Risk results by zone by group by envt risk factor
#' @description Make table of Relative Risk results by zone by group by envt risk factor.
#'   See source code for notes on this work.
#' @param mydat Data.frame of input data, one row per geographic unit such as US Census block groups, or tracts.
#' @param Enames names of columns with environmental risk factor data, default is names.e from ejscreen package
#' @param Dnames names of columns with percent (fraction) of population that is in each given demographic group, default is names.d from ejscreen package
#' @param popcolname name of column with total population count, default is pop
#' @param Zcolname name of column with name of zone such as US State name
#' @param digits Default is 4. How many significant digits to use.
#' @param testing default is FALSE
#' @return Compiles RR values in array of 3 dimensions: \code{RRS[Dnames, Enames, Zcolnames]}
#'   Returns a matrix with
#'   one demographic group per row,
#'   one environmental risk indicator per column, and
#'   third dimension for which zone (e.g., which US State)
#' @template seealsoRR
#' @examples
#'     # (This is very slow right now)
#'     # Ratios <- ejanalysis::RR.table(bg21plus, Enames = names.e, Dnames = c(names.d, names.d.subgroups), popcolname = 'pop', digits = 2)
#'     # MeansByGroup_and_Ratios <- ejanalysis::RR.means(subset(bg21plus, select=names.e), subset(bg21plus, select = c(names.d, names.d.subgroups)), bg21plus$pop)
#' data(bgtest, package = 'ejanalysis')
#' RRS.US  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = c(names.d, names.d.subgroups.pct),
#'  popcolname = 'pop')
#' RRS.ST  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = c(names.d, names.d.subgroups.pct),
#'  popcolname = 'pop', Zcolname = 'ST')
#' RRS <- RR.table.add(RRS.ST, RRS.US)
#' RRS['pctlowinc', , ]
#' RRS[ , , 'CA']  # RRS[,,'PR']
#' RRS[ , 'pm', ]
#' RRS.REGION  <- RR.table(mydat = bgtest, Enames = names.e, Dnames = c(names.d, names.d.subgroups.pct),
#'  popcolname='pop', Zcolname='REGION')
#' RRS2 <- RR.table.add(RRS, RRS.REGION)
#' RRS2[ , , '8']
#' @export
RR.table <- function(mydat, Enames=ejscreen::names.e[ejscreen::names.e %in% names(mydat)], Dnames=ejscreen::names.d[ejscreen::names.d %in% names(mydat)], popcolname='pop', Zcolname, testing=FALSE, digits=4) {

  # Compile RR values in array of 3 dimensions: RRS[Dnames, Enames, Zcolnames]
  # one Demog group per row,
  # one Envt factor per column,
  # and 3d dimension is for each zone (e.g. each state)

  ########################### #

  # TO DO LIST FOR  RR.table() :
  ########################### #

  # FIX THESE:

  #- ? options to sort results by D, E, and/or Zone; or by maxE, maxD, or maxZone?
  # Can use RR.table.sort() for that.

  #- to work on counties, where there seem to be cases where all values are NA in some slice?
  # not sure if/why it fails still
  # RIGHT NOW THIS WILL NOT HANDLE A CASE WHERE A GIVEN DEMOG GROUP HAS NA FOR THE ENTIRE COUNTY OR STATE? IS THAT EVEN POSSIBLE?

  #- to use zone data and other data vectors, not colnames.
  # Could recode this so parameters would be vectors with data rather than names of columns in mydata data.frame
  # That would make it easier to specify more complex / useful zones, for example,
  # or for case where zone factor and E and D and pop are in separate variables, not all in one data.frame

  #- to better handle a single zone.****

  #- option of saving to csv (done via write.RR.tables() now )

  #- ? have a way to add a zone that is the combination of all the specified zones,
  # so you can specify States as zones and it will add the entire US as a zone?
  #   THAT NOW CAN BE DONE VIA RR.table.add()

  ######################### #
  # - to do summary stats: **********
  ######################### #

  # - what E has the highest RRs "usually" or at least "most often"  (which E has max RR for the most Ds in given zone,
  #  then for the most zones for given D, then overall= which E has more max RRs of all D-Z combos?)?,
  # - what D has the highest RRs (same breakdowns as for what E)
  # - what zone has  highest RRs (same breakdowns)

  # You can Show 3 slices:
  #
  # 1. what is the E with highest RR, for each group in each zone?
  # 2. what is the D with highest RR, for each E in each zone?
  # 3. what is the Z with highest RR, for each group for each E?
  #
  # 4. which EZ combo has the highest RR, for each D? (18 D)
  # 5. which DZ combo has the highest RR, for each E? (13 E with 12 and a summary E)
  # 6. which DE combo has the highest RR, for each Z? (52 Z with States/DC & USA) e.g., in CA,
  #
  # in other words...
  #
  # for each D, see table of Z X E RR values. get colMaxs and rowMaxs, i.e.
  # for each Z which E has max RR, (worst E for each DZ combo)  = "worst E" has dz RRs
  # for each E which Z has max RR; (worst Z for each DE combo)  = "worst Z" has de RRs
  # which EZ combo has max RR. (worst EZ combo for each D)  has d RRs
  #
  # for each E,  see table of Z X D RR values. get colMaxs and rowMaxs, i.e.
  # for each Z which D has max RR, (worst D for each EZ combo)  = "worst D"
  # for each D which Z has max RR; (worst Z for each DE combo)  = "worst Z"
  # which DZ combo has max RR. (worst DZ combo for each E) has e RRs
  #
  # for each Z,  see table of D X E RR values. get colMaxs and rowMaxs, i.e.
  # for each D which E has max RR, (worst E for each DZ combo)  = "worst E"
  # for each E which D has max RR; (worst D for each EZ combo)  = "worst D"
  # which DE combo has max RR. (worst DE combo for each Z) has z RRs
  #
  # Full list would be
  #    D X Z X E
  #                (including for Z=whole USA, not just portions of USA)
  # For 51 States + USA, almost 18 D, 12 E plus wtd Esum =
  #  sum(52 * 18, 51 * 13, 18 * 13, 52, 18, 13 )
  #[1] 1916 RRs, most with two identified elements of pair, like "traffic for hispanics",
  # plus the RR value itself, or something like 5,000 elements of results, which is absurd.
  # We can't possible absorb results that tell us for every single state,
  #  for each E, what is the Demog group that has the worst RR, & then
  #  for each D, what is the E with worst RR, etc.

  ############## #

  if (!missing(Zcolname)) {
    if (length(Zcolname) > 1) {stop('Zcolname must be a single character string')}
    if (!(Zcolname %in% names(mydat))) {
      Znames <- Zcolname
    } else {
      Znames <- unique(mydat[ , Zcolname])
    }
  } else {
    Znames <- 'USA'
  }

  RRS <- array(NA, c(length(Dnames) + 1, length(Enames) + 1, length(Znames)))

  for (myzone.i in 1:length(Znames)) {

    # RIGHT NOW THIS WILL NOT HANDLE A CASE LIKE PR, PUERTO RICO,
    # WHERE A GIVEN DEMOG GROUP LIKE pcthisp HAS NA FOR
    # THE ENTIRE COUNTY OR STATE?

    if (!missing(Zcolname)) {
      inzone <- (mydat[ , Zcolname] == Znames[myzone.i])
    } else {
      inzone <- rep(TRUE, length(mydat[ , popcolname]))
    }

    # Note: ***** This used  na.rm in one max but not other,
    # to handle states where some E is missing always,
    # but still show max RR of the E's with valid data

    x <- RR(mydat[inzone , Enames], mydat[inzone, Dnames], mydat[inzone, popcolname])
    z <- round(rbind(myzone.max = analyze.stuff::colMaxs(x, na.rm = FALSE), x), digits)

    x <- cbind(myzone.max = analyze.stuff::rowMaxs(z, na.rm = TRUE), z)
    # rownames(x)[1] <- paste(Znames[myzone.i], '.maxD', sep = '')
    # colnames(x)[length(colnames(x))] <- paste(Znames[myzone.i], '.maxE', sep = '')
    RRS[ , , myzone.i] <- x
  }

  dimnames(RRS) <- list(d = c('max.D', Dnames), e = c('max.E', Enames), zone = Znames)

  RRS <- RR.table.addmaxzone(RRS, testing = testing)

  return(RRS)
}

