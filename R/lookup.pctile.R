lookup.pctile <- function(myvector, varname.in.lookup.table, lookup, zone) {

  #	*** FUNCTION TO LOOK UP CURRENT APPROX WEIGHTED PERCENTILES IN lookup table called us IF ALREADY IN MEMORY

	if (missing(lookup) & (exists('us'))) {lookup <- us}
	if (missing(lookup) & !exists('us')) {stop('must specify lookup= or have it in memory named "us"')}
  if (missing(zone) & lookup$REGION[1]!='USA') {stop('If lookup is not us, need to specify zone="NY" for example')}
	# lookup table must have PCTILE field (& this removes the row called 'mean')
	if (!('PCTILE' %in% names(lookup))) {stop('lookup must have a field called "PCTILE" that contains quantiles/percentiles')}
	lookup <- lookup[lookup$PCTILE!="std.dev", ]
	lookup <- lookup[lookup$PCTILE!="mean", ]
	
	if (missing(zone)) {
		return(as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , varname.in.lookup.table]) ]))
	} else {
	  if (!(zone %in% lookup$REGION)) {stop('zone not found in lookup')}
	  lookup <- lookup[lookup$REGION==zone, ]
	  return(as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , varname.in.lookup.table]) ]))
	}
	
	
	#example/usage:
	#  places.etc$new.pctile.pm <- lookup.pctile(places.etc$countymean, "PM25")
	}
	
