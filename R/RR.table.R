RR.table <- function(mydat, Enames, Dnames, popcolname, Zcolname, testing=FALSE, digits=4) {
  
  # Compile RR values in array of 3 dimensions: RRS[Dnames, Enames, Zcolnames]
  # one Demog group per row, 
  # one Envt factor per column, 
  # and 3d dimension is for each zone (e.g. each state)

##############
############################

# TO DO LIST FOR  RR.table() :
############################


# FIX THESE:

#- options to sort results by D, E, and/or Zone; or by maxE, maxD, or maxZone.

#- to work on counties, where there seem to be cases where all values are NA in some slice? not sure why it fails
# RIGHT NOW THIS WILL NOT HANDLE A CASE WHERE A GIVEN DEMOG GROUP HAS NA FOR THE ENTIRE COUNTY OR STATE? IS THAT EVEN POSSIBLE?

#- to use zone data and other data vectors, not colnames. # Could recode this so parameters would be vectors with data rather than names of columns in mydata data.frame
  # That would make it easier to specify more complex / useful zones, for example, or for case where zone factor and E and D and pop are in separate variables, not all in one data.frame

#- to better handle a single zone.****

#- option of saving to csv (done via write.RR.tables() now )

#- have a way to add a zone that is the combination of all the specified zones, so you can specify States as zones and it will add the entire US as a zone.
#   THAT NOW CAN BE DONE VIA RR.table.add()

##########################
# - to do summary stats: **********
##########################

# - what E has the highest RRs "usually" or at least "most often" (which E has max RR for the most Ds in given zone, then for the most zones for given D, then overall= which E has more max RRs of all D-Z combos?)?, 
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
#[1] 1916 RRs, most with two identified elements of pair, like "traffic for hispanics", plus the RR value itself, or something like 5,000 elements of results, which is absurd.
# We can't possible absorb results that tell us for every single state, 
#  for each E, what is the Demog group that has the worst RR, & then 
#  for each D, what is the E with worst RR, etc.


  ###############


  if (!missing(Zcolname)) {
    if (length(Zcolname) > 1) {stop('Zcolname must be a single character string')}
    if(!(Zcolname %in% names(mydat))) {
      Znames <- Zcolname
      } else {
        Znames <- unique(mydat[ , Zcolname])
      }
  } else {
    Znames <- 'USA'
  }

RRS <- array(NA, c(length(Dnames) + 1, length(Enames) + 1, length(Znames)))
  
  for (myzone.i in 1:length(Znames) ) {

    # RIGHT NOW THIS WILL NOT HANDLE A CASE WHERE A GIVEN DEMOG GROUP HAS NA FOR THE ENTIRE COUNTY OR STATE? IS THAT EVEN POSSIBLE?
    
    if (!missing(Zcolname)) {
      inzone <- (mydat[ , Zcolname]==Znames[myzone.i])      
    } else {
      inzone <- rep(TRUE, length(mydat[ , popcolname]))
    }

    # Note: This uses na.rm in one max but not other, to handle states where some E is missing always, but still show max RR of the E's with valid data

    x <- RR(mydat[inzone , Enames], mydat[inzone , Dnames], mydat[inzone, popcolname])
    z <- round(rbind(myzone.max=colMaxs(x, na.rm=FALSE), x), digits)
    x <- cbind(myzone.max=rowMaxs(z), z)
    # rownames(x)[1] <- paste(Znames[myzone.i], '.maxD', sep='')
    # colnames(x)[length(colnames(x))] <- paste(Znames[myzone.i], '.maxE', sep='')
    RRS[ , , myzone.i] <- (x)
  }
  
  dimnames(RRS) <- list(c('max.D', Dnames), c('max.E', Enames), Znames)
  
  ######################
  # Add max.zone slices
  ######################
  
  RRTEMP <- array(0, dim(RRS) + c(0, 0, 1))
  dimnames(RRTEMP) <- list(unlist(dimnames(RRS)[1]), unlist(dimnames(RRS)[2]), c('max.zone', unlist(dimnames(RRS)[3])) )  
  if (testing) { 
    print('structure of RRTEMP currently is this: '); print(str(RRTEMP))
    print('structure of RRS currently is this: '); print(str(RRS)) 
  }

    
    for (i in 1:length(RRS[1,,1])) {
      if (testing) { 
        cat('i = ', i, '\n')
        cat('RRS[ , i, ] is this now:\n'); print(RRS[ , i, ] )
      }
      
      if (length(Znames)==1) {
        # only one zone exists. max.zone is redundant, but just keep it there anyway, partly to simplify merging with a multizone table.
        RRTEMP[ , i, ] <- cbind(max.zone=RRS[ , i, 1, drop=FALSE], RRS[ , i, 1, drop=FALSE])
      } else {
        RRTEMP[ , i, ] <- cbind(max.zone=rowMaxs(RRS[ , i, , drop=FALSE]) , RRS[ , i, 1:length(Znames), drop=FALSE])
      }
      
      if (testing) {
        cat('RRTEMP[ , i, ] is this now:\n'); print(RRTEMP[ , i, ] )
      }
    }
    # that warned of problem in row names duplications at some point


  RRS <- RRTEMP
  
  return(RRS)
  
  # EXAMPLE OF SPECIFYING VARIABLES TO PASS:
  RRS.REGION <- RR.table(mydat=bg, Enames=names.e, Dnames=c(names.d, names.d.subgroups.pct), popcolname='pop', Zcolname='REGION')
}

