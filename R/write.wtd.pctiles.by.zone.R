#' @title create lookup table as file of pop-weighted or unwtd percentiles, mean, sd for US or by state or region
#'
#'
#' @description   also see \cr\cr
#'   ejscreen.lookuptables  and \cr\cr
#'   write.wtd.pctiles.by.zone   and \cr\cr
#'   table.pop.pctile  and \cr\cr
#'   map service with lookup tables \cr\cr \cr\cr
#' \preformatted{
#' }
#'
#' @param mydf data.frame with numeric data. Each column will be examined to calculate mean, sd, and percentiles, for each zone.
#' @param wts optional vector of numbers such as population counts as weights, as long as nrow(mydf)
#' @param filename prefix to use for filename to be saved locally
#' @param zone.vector optional names of states or regions, for example. same length as wts, or rows in mydf
#' @param zoneOverallName optonal If not by zone, name of entire domain to use in table column called REGION. Default is USA.
#' @examples
#'   \dontrun{
#'   bg = ejscreen::bg21
#'   pctilevariables <- c(names.e, names.d, names.ej)
#'    ejanalysis::write.wtd.pctiles(mydf = bg[ , pctilevariables], wts = bg$pop, filename =  'lookupUSA')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$REGION, filename =  'lookupRegions')
#'    ejanalysis::write.wtd.pctiles.by.zone(mydf = bg[ , pctilevariables], wts = bg$pop,
#'                                    zone.vector = bg$ST,     filename =  'lookupStates')
#'   }
#'
#' @export
write.wtd.pctiles.by.zone <- function(mydf, wts=NULL, filename, zone.vector=NULL, zoneOverallName='USA') {

  # FUNCTIONS TO SHOW percentile STATS STRATIFIED BY REGION (OR STATE),
  #	SAVING A FILE OF STATS FOR EACH REGION OR STATE
  #  LIKE A SET OF LOOKUP TABLES

  # but now will fix it to just create a single table or file for the whole group of zones like all States in 1 csv file, as EJScreen would use.

  if (is.null(zone.vector)) {
    if (is.null(wts)) {

      # NO WEIGHTS NO ZONES ####

      r <- data.frame(sapply(mydf, function(x) pctiles.exact(x)))
      r <- rbind(r, t(data.frame(mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
      r <- rbind(r, t(data.frame(std.dev = sapply(mydf, function(x) sd(x, na.rm = TRUE)))))
      r$REGION <- zoneOverallName
    } else {

      # WEIGHTS, BUT NO ZONES ####

      r = data.frame(sapply(mydf, function(x) analyze.stuff::wtd.pctiles.exact(x, wts) ) )
      r = rbind(r, t(data.frame(mean=sapply(mydf, function(x) Hmisc::wtd.mean(x, wts, na.rm=TRUE) ) ) ))
      r = rbind(r, t(data.frame(std.dev=sapply(mydf, function(x) sqrt(Hmisc::wtd.var(x, wts, na.rm=TRUE)) ) ) ))
      r$REGION <- zoneOverallName
    }
  } else {            ## ZONES
    r <- list()
    for (i in 1:length(unique(zone.vector))) {
      z <- unique(zone.vector)[i]
      if (is.null(wts)) {

        # ZONES BUT NO WEIGHTS ####

        r[[i]] <- data.frame(sapply(mydf[zone.vector==z,  ], function(x) pctiles.exact(x)))
        r[[i]] <- rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector==z,  ], function(x) mean(x, na.rm = TRUE)))))
        r[[i]] <- rbind(r[[i]], t(data.frame(std.dev = sapply(mydf[zone.vector==z,  ], function(x) sd(x, na.rm = TRUE)))))
        r[[i]]$REGION <- z
      } else {

        # ZONES AND WEIGHTS ####

        r[[i]] = data.frame(sapply(mydf[zone.vector==z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector==z]) ) )
        r[[i]] = rbind(r[[i]], t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) Hmisc::wtd.mean(x, wts[zone.vector==z], na.rm=TRUE) ) ) ))
        r[[i]] = rbind(r[[i]], t(data.frame(std.dev=sapply(mydf[zone.vector==z,  ], function(x) sqrt(Hmisc::wtd.var(x, wts[zone.vector==z], na.rm=TRUE) ) )) ))
        r[[i]]$REGION <- z
      }
    }
    # ZONE LOOP DONE
    r <- do.call(rbind, r)

  }
  r <- data.frame(OBJECTID = 1:NROW(r), REGION = r$REGION, PCTILE =  gsub( '%', '',trimws(rownames(r))), r[ , 'REGION' == colnames(r)], stringsAsFactors = FALSE)
  write.csv(r, file = paste(filename, ".csv", sep = ""))
  invisible(r)

}



