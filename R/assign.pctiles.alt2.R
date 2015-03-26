#' @title Assign percentiles to values (alternative formula)
#'
#' @description For each column look at the distribution of values across all rows, 
#' and find what percentile a given value is at.
#' @details Assign percentile as cumulative sum of (the weights ranked by the value x).
#' Then fixes ties.
#' 	# Could also add parameter like in rank(), na.last, 
#' defining na.rm but also where to rank NA values if included, etc.
#' Default now is like na.last=NA, but like na.last='last' if na.rm=FALSE
#' Could also add parameter like in rank(), ties.method, defining if ties get min, max, or mean of percentiles initially assigned to ties.
#' Default for ties right now is like ties.method=max (which might not be what assign.pctiles() does in fact).
#' @param x vector or data.frame
#' @param weights Optional, NULL by default (not fully tested), vector of weights for weighted percentiles (e.g., population weighted).
#' @param na.rm Logical, optional, TRUE by default. Should NA values (missing data) be removed first to get percentile of those with valid data.
#'   If FALSE, NA values are treated as being at the high percentiles.
#' @param zone Optional, NULL by default, not yet implemented.
#' @return Returns a numeric vector or data.frame same size as x.
#' @template seePctiles
#' @examples
#' x <- c(30, 40, 50, 12,12,5,5,13,13,13,13,13,8,9,9,9,9,9,10:20,20,20,20,21:30); weights <- rep(c(2,3), length(x)/2)
#' cbind(weights, x, PCTILE=assign.pctiles.alt2(x,weights))
#' 
#' # PERCENTILE OF ALL, NOT JUST THOSE WITH VALID DATA, IF na.rm=FALSE, but then NA values preclude high percentiles:
#' x <- c(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA,13,13,8,9,9,9,9,9,10:20,20,20,20,21:30); weights <- rep(c(2,3), length(x)/2)
#' cbind(weights, x, PCTILE.alt2=assign.pctiles.alt2(x, weights, na.rm=FALSE), pctile=assign.pctiles(x,weights))[order(x),]
#' cbind(weights, x, PCTILE.alt2=assign.pctiles.alt2(x, weights, na.rm=TRUE), pctile=assign.pctiles(x,weights))[order(x),]
#' 
#' V=9
#' sum(weights[!is.na(x) & x <= V]) / sum(weights[!is.na(x)])
#'
#' #A value (V) being at this PCTILE% means that (assuming na.rm=TRUE):
#' 
#' # V >= x  for        PCTILE% of weights     (for non-NA x), so 
#' # V < x   for 100% - PCTILE% of weights     (for non-NA x), or
#' # PCTILE% of all weights have V >= x (for non-NA x), so
#' # 100% - PCTILE% of all weights have V < x  (for non-NA x).
#'
#' x <- c(32, NA, NA, NA,NA,NA,NA,NA,NA,NA,13,13,8,9,9,9,9,9,10:20,20,NA,20,21:30); weights <- rep(c(2,3), length(x)/2)
#' cbind(weights, x, PCTILE.alt2=assign.pctiles.alt2(x, weights, na.rm=FALSE), pctile=assign.pctiles(x,weights))[order(x),]
#' cbind(weights, x, PCTILE.alt2=assign.pctiles.alt2(x, weights, na.rm=TRUE), pctile=assign.pctiles(x,weights))[order(x),]
#' @export
assign.pctiles.alt2 <- function(x, weights=NULL, na.rm=TRUE, zone=NULL) { 

  # weights=NULL is untested.
	
	#########
	# assign percentile as cumulative sum of (the weights ranked by the value x)
	#########

	i <- order(x)
	original <- order((1:length(x))[i])

	if (na.rm) {
		weights[is.na(x)] <- 0
	}

	weights.pct <- weights / sum(weights)
	pctiles <- cumsum(weights.pct[i])

	#print(cbind(x,weights,weights.pct))

	#########
	# fix ties via aggregate()
	#########

	# aggregate applied to all x can be slow, since it calculates max for every nontied as well, but it works
	# aggregate gives only sorted list of unique x values, so need to expand to x length, via match (or merge could work too)

	pctile.with.ties.as.max <- aggregate(pctiles, by=list(x[i]), max)
	pctiles2 <- pctile.with.ties.as.max$x[match(x, pctile.with.ties.as.max[ , 1])][i]

	return(pctiles2[original])
}

###############################################
if (1==0) {

	#	NOTE: THIS IS HOW rank() WORKS (from R help):
	#
	# rank(x, na.last = TRUE,
	#      ties.method = c("average", "first", "random", "max", "min"))
	# Arguments
	# 
	# x	
	# a numeric, complex, character or logical vector.
	# 
	# na.last	
	# for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed; if "keep" they are kept with rank NA.
	# 
	# ties.method	
	# a character string specifying how ties are treated, see ‘Details’; can be abbreviated.
	# 
	# Details
	# 
	# If all components are different (and no NAs), the ranks are well defined, with values in seq_len(x). With some values equal (called ‘ties’), the argument ties.method determines the result at the corresponding indices. The "first" method results in a permutation with increasing values at each index set of ties. The "random" method puts these in random order whereas the default, "average", replaces them by their mean, and "max" and "min" replaces them by their maximum and minimum respectively, the latter being the typical sports ranking.
	# 
	# NA values are never considered to be equal: for na.last = TRUE and na.last = FALSE they are given distinct ranks in the order in which they occur in x.
	# 

	#  THIS ALT WAY OF ASSIGNING PERCENTILES IS IDENTICAL TO THE DEFAULT/MAIN ONE USED IN screening, OTHER THAN NEGLIGIBLE DIFFERENCE DUE TO HOW NUMERIC VALUES ARE STORED:
	#
	# 	similar( assign.pctiles(bg.ftp$traffic.score, bg.ftp$pop), assign.pctiles.alt2(bg.ftp$traffic.score, bg.ftp$pop, na.rm=TRUE),99.9999999)
	# 		   out
	# 	1 217486
	# sum(!is.na(bg.ftp$pm))
	# [1] 215491
	# 
	# 	table( assign.pctiles(bg.ftp$traffic.score, bg.ftp$pop) - assign.pctiles.alt2(bg.ftp$traffic.score, bg.ftp$pop, na.rm=TRUE), useNA='always')
	# 
	# 	-1.11022302462516e-16 -5.55111512312578e-17 -2.77555756156289e-17 -1.38777878078145e-17 -6.93889390390723e-18 -3.46944695195361e-18 -1.73472347597681e-18 
	# 					   12                   104                    58                    16                    32                    12                    23 
	# 	-8.67361737988404e-19 -4.33680868994202e-19 -2.16840434497101e-19  -1.0842021724855e-19 -5.42101086242752e-20                     0    8.470329472543e-22 
	# 					   14                     9                     2                     1                     1                213780                     1 
	# 	  1.6940658945086e-21   3.3881317890172e-21  1.35525271560688e-20  2.71050543121376e-20  3.46944695195361e-18  6.93889390390723e-18  1.38777878078145e-17 
	# 						1                     2                     1                     3                     1                     3                   132 
	# 	 2.77555756156289e-17  5.55111512312578e-17  1.11022302462516e-16                  <NA> 
	# 					   96                   192                  2990                   253 
}
