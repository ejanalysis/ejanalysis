#' @title Calculate environmental justice (EJ) index for each place
#'
#' @description Create an index that combines environmental and demographic indicators for each Census unit (e.g., block group).
#' @details Creates one EJ index column for each environmental indicator column,
#'  or if given a vector or single column of environmental indicators instead of data.frame, returns a vector or column.
#'  Each "place" can be a Census unit such as a State, County, zip code, tract, block group, block, for example (or even by individual if person-level data are available).
#'
#'  Note: For 1.5, 3.5, 4.5 need vector d.avg.all.elsewhere. calculated in each of those.
#'  *** But does not properly handle cases where us.demog or universe.us.demog specified because denominator is not same as pop
#'  *** and need to fix for when  is.na(p) | is.na(demog) ****
#'  Type 2 however does handle NA values appropriately, meaning a result for a given col of env.df is set to NA for a given row (assuming na.rm=FALSE) if and only if NA is found in that row, in demog or weights or that one col of env.df.
#'
#' @param env.df Environmental indicators vector or numeric data.frame, one column per environmental factor, one row per place (e.g., block group).
#' @param demog Demographic indicator(s) vector or data.frame, numeric fractions of population that is in specified demographic group (e.g., fraction below poverty line), one per place.
#' @param us.demog Optional number specifying overal area-wide value for demog (e.g., US percent Hispanic). Default is to calculate it as weighted mean of demog,
#'   where the weights are universe.us.demog if specified, or else just 'weights'.
#'   If the weights are population counts and demog is percent Hispanic, for example, us.demog is the percent  of US population that is Hispanic.
#' @param weights Optional, and default is equal weighting. If us.demog and universe.us.demog are not specified, weights are used to find
#'   weighted mean of demog to use as area-wide overall average (e.g., population-weighted average percent Hispanic, for all block groups in USA). One weight per place.
#' @param universe.us.demog Optional numeric vector. If specified and us.demog not specified, used instead of weights to get weighted mean of demog to find area-wide demog.
#'   This should be the actual denominator, or universe, that was used to create percent demog --
#'   universe.us.demog if specified should be a vector that has the count, for each place, of the denominator for finding the US overall percent  and this may be slightly different than total population.
#'   For example if demog=places$pctlowinc then true universe.us.demog=places$povknownratio which is the count for whom poverty ratio is known in each place, which is <= pop.
#' @param prefix Optional character string used as first part of each colname in results. Default is "EJ.DISPARITY."
#' @param type Specifies type of EJ Index. Default is type=1. Several formulas are available: \cr
#' \itemize{
#'  \item For type=1,   ej.indexes = weights * env.df * (demog  - us.demog)  ## This is the EJ Index in EJSCREEN 2015. Note: us.demog could also be called d.avg.all, and ** note that na.rm is currently ignored for type=1
#'  \item For type=1.5, ej.indexes= weights * env.df * (demog  - d.avg.all.elsewhere) # for a place that is one of many this can be almost identical to type 1 \cr
#'  \item For type=2.1, ej.indexes = weights * demog  * (env.df - e.avg.all)  # like type 1 but env and demog roles are swapped \cr
#'  \item For type=2,   ej.indexes = weights * demog  * (env.df - e.avg.nond) \cr
#'  \item For type=2.5, ej.indexes = weights * demog  * (env.df - e.avg.nond.elsewhere )  \cr
#'  \item For type=3,   ej.indexes = weights * ( (demog * env.df) - (d.avg.all           * e.avg.nond ) )   \cr
#'  \item For type=3.5, ej.indexes = weights * ( (demog * env.df) - (d.avg.all.elsewhere * e.avg.nond.elsewhere) )   \cr
#'  \item For type 4  , ej.indexes = weights * ( (demog - d.avg.all          ) * (env.df - e.avg.nond ) ) \cr
#'  \item For type=4.5, ej.indexes = weights * ( (demog - d.avg.all.elsewhere) * (env.df - e.avg.nond.elsewhere) ) \cr
#'  \item For type=5  , ej.indexes = weights * env.df * demog ## A "Population Risk" or "Burden" index = Number of cases among D group if e is individual risk, or just "people-points among D" if e is "points" \cr
#'  \item For type=6,   ej.indexes = env.df * demog ## A "percent-based" indicator = percent in group D times envt indicator. \cr
#' }
#'  where
#' \itemize{
#'  \item us.demog = overall demog where avg person lives (pop wtd mean of demog). This may be almost exactly the same as d.avg.all.elsewhere \cr
#'  \item d.avg.all            = overall value for d as fraction of entire population (including the one place being analyzed). \cr
#'  \item d.avg.all.elsewhere  = overall value for d as fraction of entire population other than the one place being analyzed. \cr
#'  \item e.avg.all            =  avg environmental indicator value for average person  \cr
#'  \item e.avg.nond           =  avg environmental indicator value for average person who is not in the D-group, among all (including the one place being analyzed). This is typically the expected as opposed to observed value of e within group D, in the context of EJ analysis of disparity in e. \cr
#'  \item e.avg.nond.elsewhere =  avg environmental indicator value for average person who is not in the D-group, among all except in the one place being analyzed. \cr
#' }
#' @return Returns a numeric data.frame (or matrix if as.df=FALSE) of EJ indexes, one per place per environmental indicator.
#' @examples
#'  statedat <- data.frame(state.x77)
#'  hist(myej <- ej.indexes(env.df=statedat[ , c('Life.Exp', 'Frost')],
#'   demog=statedat$HS.Grad/100, weights=statedat$Population, prefix='EJtype1.', type=1 ))
#'  set.seed(999)
#'  myej <- ej.indexes(env.df=rnorm(1000, 10, 3), demog=runif(1000, 0, 1),
#'   weights=runif(1000, 500, 5000))
#'  myej
#' @export
ej.indexes <- function(env.df, demog, weights, us.demog, universe.us.demog, as.df=TRUE, prefix="EJ.DISPARITY.", type=1, na.rm=FALSE) {

  # Check for missing or bad parameters
  validtypes <- c(1, 1.5, 2, 2.1, 2.5, 3, 3.5, 4, 4.5, 5, 6)
  if (!(type %in% validtypes)) {stop('invalid value specified for type')}

  if (!is.data.frame(env.df) ) { env.df <- as.data.frame(env.df)}

  if (missing(env.df))  {stop('Error - must specify env= vector or data.frame of environmental values\n') }
  if (missing(demog))   {stop('Error - must specify demog= vector for demographic group as fraction of population\n')}
  if (missing(weights)) {
    cat('Warning - did not specify weights= vector of population counts, so assumed all are equal\n')
    weights <- rep(1, NROW(env.df))
  }

  if (!all(sapply(env.df, class) %in% c('numeric','integer') )) {stop('env.df must be a data.frame of numeric fields')}
  if (any(is.na(env.df))) {cat('Warning - NA values in env\n') }
  if (any(is.na(demog))) {cat('Warning - NA values in demog\n') }
  if (any(is.na(weights))) {cat('Warning - NA values in weights\n') }

  if (missing(us.demog)) {
    cat('Warning - Did not specify us.demog= fraction of US population that is in the given demographic group\n')

    # NOTE: If us.demog (overall US percent  for demographic indicator) has to be calculated, it is done for all places with valid (non-NA) weights or universe.us.demo values, including any where env.df is NA/missing,
    # so it may differ from the us.demog that would correspond to only places with valid env.df scores.
    # That shouldn't matter in most cases, since the ej.index is used in a relative sense, but the reported USpercent  overall here will not necessarily be the USpercent  just among places with valid EJ indexes.

    if (missing(universe.us.demog)) {
      # if universe.us.demog not specified, then assume population count (or whatever specified weights used) was the right denominator for percent demog & use it to find US percent demog

      # BUT NOTE I FIND DISCREPANCIES BETWEEN
      # these 2 ways, and should switch to former if possible, since it seems more accurate and simple, but need original counts numerators and denominators:
      #  VSI.US <-  with(bg.ftp, ( sum(mins)/sum(pop) + sum(lowinc)/sum(povknownratio) ) / 2 )
      # VERSUS
      #  VSI.US2 <- weighted.mean(VSI.eo, (bg.ftp$pop + bg.ftp$povknownratio)/2)  # ALSO NOTE na.rm should be explicit
      # Issue is that VSI.eo was calculated as mean of pctlowinc and pctmin where pctlowinc was lowinc/povknownratio and pctmin was mins/pop
      # so it is not quite the correct weighting if you do Hmisc::wtd.mean(VSI.eo, pop)

      us.demog <- Hmisc::wtd.mean(demog, weights, na.rm=TRUE) # NOTE THAT  na.rm=TRUE & might make that a parameter of ej.indexes() but unsure when that might be needed
      # *** WHEN FINDING THE US AVG HERE, DO YOU WANT TO EXCLUDE PLACES THAT HAVE NA FOR POP? FOR SOME E? BECAUSE THOSE PLACES ARE LEFT OUT OF EJ INDEX CALC IF na.rm=TRUE IN OVERALL FUNCTION CALL.
    } else {
      # if universe.us.demog IS specified, then use it as the right denominator (that was used to create percent demog) & use it to find US percent demog
      us.demog <- Hmisc::wtd.mean(demog, universe.us.demog, na.rm=TRUE) # NOTE THAT  na.rm=TRUE & might make that a parameter of ej.indexes() but unsure when that might be needed
      # *** WHEN FINDING THE US AVG HERE, DO YOU WANT TO EXCLUDE PLACES THAT HAVE NA FOR POP? FOR SOME E? BECAUSE THOSE PLACES ARE LEFT OUT OF EJ INDEX CALC IF na.rm=TRUE IN OVERALL FUNCTION CALL.

      cat('Using the specified universe.us.demog to find the overall percent  demog in all locations with valid demographics, which may be a bit different than those with valid envt scores\n')
    }

    cat(paste('Using calculated us.demog=', us.demog, ', based on all locations with valid demographics (which may be a bit different than those with valid envt scores)\n') )
  }


  if (length(us.demog) > 1) {stop('Error - if specified, us.demog must be a single number, fraction of overall pop that is in given demog group\n') }
  if (is.na(us.demog) || us.demog < 0 || us.demog > 1 || !is.numeric(us.demog) ) {stop('Error - if specified, us.demog must be a single number, fraction of overall pop that is in given demog group\n') }

  if (any(!is.numeric(env.df[!is.na(env.df)])) ) {stop('Error - env.df must contain numbers or NA values\n') }
  if (any(is.na(env.df))) {cat('Warning - NA values in env\n') }
  if (any(is.na(demog))) {cat('Warning - NA values in demog\n') }
  if (any(demog[!is.na(demog)] < 0) || any(demog[!is.na(demog)] > 1) || any(!is.numeric(demog[!is.na(demog)])) ) {stop('Error - if specified, demog must be a vector of numbers or NA values, for each place it is the fraction of pop that is in given demog group\n') }
  if (any(is.na(weights))) {cat('Warning - NA values in weights\n') }
  if (length(unique( length(demog), length(env.df[ , 1]), length(weights))) > 1) {cat('Warning - demog, weights, & 1st column of env.df are not all the same length!')}


  ################################################
  # *** EVEN IF na.rm = TRUE, CAN'T JUST REMOVE EVERY PLACE WHERE DEMOG OR WEIGHTS (POP) IS NA ! WE WANT TO RETURN NA VALUES THERE
  ################################################
  #
  dpnotna <- (!is.na(demog) & !is.na(weights))
  #   if (na.rm = TRUE) {
  #     demog <- demog[dpnotna]
  #     weights <- weights[dpnotna]
  #     env.df <- env.df[dpnotna, ]
  #   }


  ###################################
  # specify type of formula to use
  ###################################

  # For type=1, ej.indexes = weights * env.df * (demog  - us.demog)  # us.demog could also be called d.avg.all
  # For type=1.5 ej.indexes= weights * env.df * (demog  - d.avg.all.elsewhere) # for a place that is one of many this can be almost identical to type 1

  # For type=2.1, ej.indexes = weights * demog  * (env.df - e.avg.all)  # like type 1 but env and demog roles are swapped
  # For type=2,   ej.indexes = weights * demog  * (env.df - e.avg.nond)
  # For type=2.5, ej.indexes = weights * demog  * (env.df - e.avg.nond.elsewhere )

  # For type=3,   ej.indexes = weights * ( (demog * env.df) - (d.avg.all           * e.avg.nond ) )
  # For type=3.5, ej.indexes = weights * ( (demog * env.df) - (d.avg.all.elsewhere * e.avg.nond.elsewhere) )

  # For type=4  , ej.indexes = weights * ( (demog - d.avg.all          ) * (env.df - e.avg.nond ) )
  # For type=4.5, ej.indexes = weights * ( (demog - d.avg.all.elsewhere) * (env.df - e.avg.nond.elsewhere) )

  # For type=5  , ej.indexes = weights * env.df * demog

  # For type=6,   ej.indexes = env.df * demog



  if (type==1 | type==1.5) {

    # For type=1, ej.indexes = weights * env.df * (demog  - us.demog)  # us.demog could also be called d.avg.all
    # For type=1.5 ej.indexes= weights * env.df * (demog  - d.avg.all.elsewhere) # for a place that is one of many this can be almost identical to type 1

    ejfunction <- function(e, d, p, na.rm=TRUE) {

      if (type==1) {
        enotna <- !is.na(e)
        d.ref <- us.demog  # has been provided or calculated already
      } else {
        #d.avg.all.elsewhere <- d.avg.all.elsewhere.calc(p, demog)


        # *** REVIEW THE NA HANDLING HERE ****

        # Note: the type 1 formula will return NA when any value in the equation is NA, ignoring na.rm, ????
        # so a block group EJ index for ENV1 would be NA if ENV1, weights, or demog is NA there.

        #
        # BUT also COULD POSSIBLY WANT TO REMOVE FROM SUM THOSE WHERE E OR P OR D IS NA, IF na.rm=TRUE

        wtd.d <- sum(d * p, na.rm=na.rm) # BUT also SHOULD REMOVE FROM SUM THOSE WHERE E IS NA, IF na.rm=TRUE
        weights.d <- sum(p, na.rm=na.rm) # BUT also COULD POSSIBLY WANT TO REMOVE FROM SUM THOSE WHERE E or d IS NA, IF na.rm=TRUE
        d.avg.all.elsewhere  <- (wtd.d - (d * p)) / (weights.d - p) # BUT also COULD POSSIBLY WANT TO REMOVE FROM SUM THOSE WHERE E OR P OR D IS NA, IF na.rm=TRUE
        d.ref <- d.avg.all.elsewhere
      }
      return( p * e * (d - d.ref) )
    }
  }

  if (type==2.1 | type==2 | type==2.5) {

    # For type=2.1, ej.indexes = weights * demog  * (env.df - e.avg.all)
    # For type=2,   ej.indexes = weights * demog  * (env.df - e.avg.nond)
    # For type=2.5, ej.indexes = weights * demog  * (env.df - e.avg.nond.elsewhere ) # like type 1 but env and demog roles are swapped

    ejfunction <- function(e, d, p, na.rm=TRUE) {   # ok that default is na.rm=TRUE, since value gets passed here

      # # *** if na.rm=TRUE, THEN FOR CERTAIN CALCULATIONS only (like getting avg or reference E), EXCLUDE EVERY PLACE WHERE THIS E IS NA
      enotna <- !is.na(e)
      # if (na.rm == TRUE) {
      #   d <- d[!is.na(e)]
      #   p <- p[!is.na(e)]
      #   e <- e[!is.na(e)]
      # }

      # Ideally, most of these (those without e in them) would be done once, and don't need to do the calculation again for each environmental indicator,
      # but it may be cleaner to put them inside the function and that lets na.rm be specified when called:
      nond <- 1-d # d is fraction who are in d-group, and nond is 1-d or fraction that is not in d-group

      wtd.e <- sum(e * p * nond, na.rm=na.rm)  # NA in this E will give NA as result if na.rm=FALSE
      if (na.rm) {
        weights.e <-  sum(p[enotna] * nond[enotna], na.rm=na.rm) # THIS HELPS GIVE THE WTD MEAN OF E AMONG ONLY PLACES WHERE THIS PARTICULAR E IS VALID (NOT NA)
      } else {
        weights.e <-  sum(p * nond, na.rm=na.rm) # THIS CALCULATES THE AVG OR REF E AS WTD MEAN OF E USING NUMERATOR EXCLUDING NA PLACES BUT DENOMINATOR INCLUDING THEM, WHICH WOULD BE VERY STRANGE
      }


      # NA HANDLING NOT YET FIXED FOR TYPE 2.1 AND 2.5 !*****

      if (type==2.1) {
        e.avg.all  <- sum(e * p, na.rm=na.rm) / sum(p, na.rm=na.rm) # i.e. this is the pop wtd mean e value of all people
        e.ref <- e.avg.all
      }
      if (type==2) {
        e.avg.nond.everywhere <-  wtd.e                    /  weights.e                 # i.e. this is the pop wtd mean of nond
        e.ref <- e.avg.nond.everywhere
      }
      if (type==2.5) {
        e.avg.nond.elsewhere  <- (wtd.e - (e * p * nond) ) / (weights.e - (p * nond) )  # everywhere but this one place
        e.ref <- e.avg.nond.elsewhere
      }

      return( p * d * (e - e.ref) )
    }
  }

  if (type==3 | type==3.5) {

    # For type=3,   ej.indexes = weights * ( (demog * env.df) - (d.avg.all           * e.avg.nond ) )
    # For type=3.5, ej.indexes = weights * ( (demog * env.df) - (d.avg.all.elsewhere * e.avg.nond.elsewhere) )

    ejfunction <- function(e, d, p, na.rm=TRUE) {

      # Ideally, most of these (those without e in them) would be done once, and don't need to do the calculation again for each environmental indicator,
      # but it may be cleaner to put them inside the function and that lets na.rm be specified when called:
      nond <- 1-d # d is fraction who are in d-group, and nond is 1-d or fraction that is not in d-group

      wtd.d <- sum(d * p, na.rm=na.rm)
      weights.d <- sum(p, na.rm=na.rm)

      wtd.e <- sum(e * p * nond, na.rm=na.rm)
      weights.e <-  sum(p * nond, na.rm=na.rm)

      if (type==3) {
        # weights * ( (demog * env.df) - (d.avg.all            * e.avg.nond ) )
        e.avg.nond.everywhere <-  wtd.e                    /  weights.e                 # i.e. this is the pop wtd mean of nond
        e.ref <- e.avg.nond.everywhere
        d.avg.all.everywhere <-  wtd.d            /  weights.d
        d.ref <- d.avg.all.everywhere
      } else {
        # weights * ( (demog * env.df) - (d.avg.all.elsewhere  * e.avg.nond.elsewhere ) )
        e.avg.nond.elsewhere  <- (wtd.e - (e * p * nond) ) / (weights.e - (p * nond) )  # everywhere but this one place
        e.ref <- e.avg.nond.elsewhere
        d.avg.all.elsewhere  <- (wtd.d - (d * p)) / (weights.d - p)
        d.ref <- d.avg.all.elsewhere
      }

      return( p * ( (d * e) - (d.ref * e.ref) ) )
    }
  }

  if (type==4 | type==4.5) {

    # For type 4  , ej.indexes = weights * ( (demog - d.avg.all          ) * (env.df - e.avg.nond ) )
    # For type=4.5, ej.indexes = weights * ( (demog - d.avg.all.elsewhere) * (env.df - e.avg.nond.elsewhere) )

    ejfunction <- function(e, d, p, na.rm=TRUE) {

      # Ideally, most of these (those without e in them) would be done once, and don't need to do the calculation again for each environmental indicator,
      # but it may be cleaner to put them inside the function and that lets na.rm be specified when called:
      nond <- 1-d # d is fraction who are in d-group, and nond is 1-d or fraction that is not in d-group

      wtd.d <- sum(d * p, na.rm=na.rm)
      weights.d <- sum(p, na.rm=na.rm)

      wtd.e <- sum(e * p * nond, na.rm=na.rm)
      weights.e <-  sum(p * nond, na.rm=na.rm)

      if (type==4) {
        # weights * ( (demog * env.df) - (d.avg.all            * e.avg.nond ) )
        e.avg.nond.everywhere <-  wtd.e                    /  weights.e                 # i.e. this is the pop wtd mean of nond
        e.ref <- e.avg.nond.everywhere
        d.avg.all.everywhere <-  wtd.d            /  weights.d
        d.ref <- d.avg.all.everywhere
      } else {
        # weights * ( (demog * env.df) - (d.avg.all.elsewhere  * e.avg.nond.elsewhere ) )
        e.avg.nond.elsewhere  <- (wtd.e - (e * p * nond) ) / (weights.e - (p * nond) )  # everywhere but this one place
        e.ref <- e.avg.nond.elsewhere
        d.avg.all.elsewhere  <- (wtd.d - (d * p)) / (weights.d - p)
        d.ref <- d.avg.all.elsewhere
      }

      return( p * (d - d.ref) * (e - e.ref) )
    }
  }

  if (type==5) {

    # A "Population Risk" or "Burden" index = Number of cases among D group if e is individual risk, or just "people-points among D" if e is "points"
    # For type=5  , ej.indexes = weights * env.df * demog

    ejfunction <- function(e, d, p, na.rm=TRUE) {

      return( p * e * d )
    }
  }

  if (type==6) {

    # A "Percent-based" index (percent demographics times envt indicator, ignoring pop count)
    # For type=6,   ej.indexes = env.df * demog

    ejfunction <- function(e, d, p, na.rm=TRUE) {

      return( e * d )
    }
  }


  ###################################
  # calculate results
  ###################################

  if (length(env.df) > 1) {
    # If a data.frame (multiple columns) was passed to this function
    ej <- sapply(env.df, FUN=function(x) { ejfunction(e=x, d=demog, p=weights, na.rm=na.rm) } )
    #    ej <- sapply(env.df, FUN=function(x) { x * weights * (demog - us.demog) } ) # for basic type 1
    colnames(ej) <- paste(prefix, colnames(ej), sep="")
  } else {
    # If a single vector/single-colum data.frame (one environmental indicator) was passed to this function
    ej <- ejfunction(e=env.df, p=weights, d=demog, na.rm=na.rm)
  }

  ###################################
  # format output
  ###################################

  if (as.df) {
    ej <- as.data.frame(ej, stringsAsFactors=FALSE)
    # actually it probably already is a data.frame
  } else {
    if (length(env.df) > 1) {
      # has more than 1 column, and don't want data.frame, so provide matrix
      ej <- as.matrix(ej)
    } else {
      # just make it a simple vector since only 1 column and don't want data.frame
      ej <- as.vector(as.matrix(ej))
    }
  }
  return(ej)
}
