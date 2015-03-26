

rrf <- function(e,d,pop) {
	return((sum(pop*d*e)/sum(pop*d))/(sum(pop*(1-d)*e)/sum(pop*(1-d))))

	# VECTORIZED VERSION OF RELATIVE RISK FUNCTION FOR ONE ENVIRONMENTAL FACTOR, ALL DEMOGRAPHIC GROUPS:
	
	# where inputs are vectors over small places like Census blocks or block groups
	# or possibly individuals (ideally) but then d would be a dummy=1 for selected group and 0 for people not in selected group
	#	e is environmental index or health risk level (e.g., PM2.5 concentration to which this person or place is exposed)
	#	d is % of place that is selected demog group (e.g. % Hispanic) (or d=1 or 0 if this is a vector of individuals)
	#	pop is population count of place (or pop=1 if this is a vector of individuals)

	# rrfv <- Vectorize(rrf, vectorize.args=c("d"))  # usage: rrfv(places$pm, places[ , unlist(Dlist)],  places$pop)
}

rrfv <- Vectorize(rrf, vectorize.args=c("d"))  # usage: rrfv(places$pm, places[ , unlist(Dlist)],  places$pop)

####################

