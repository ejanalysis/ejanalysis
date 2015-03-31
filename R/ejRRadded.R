#' @title Formulas for local contributions to EJ metrics
#' @description Formulas for pop counts, pop demog pct, risk, cases, RR,
#'  excess risk, excess cases (like EJ index sum),
#'  and how those are different under 4 alt scenarios like E of D is set to that of dref.
#' @param P population count per place
#' @param D demographic group as fraction of P
#' @param E environmental indicator (e.g., risk, exposure, or any local indicator, but if it is individual risk then number of cases can be calculated rather than just people-points)
#' @export
ejRRadded   <- function(E,D,P, na.rm=TRUE, ...) {

  if (1==0) {

    E=bg$cancer
    D=bg$pctlowinc
    P=bg$pop
    na.rm=TRUE
  }

  #######################################################################################################################
  # **** This treats P as the full universe or denominator to use for D & d *****
  # ******* this does not handle zero as denominator in zone ****
  # **** need to handle NA in E here... **************
  #######################################################################################################################

  #########
  # People (population counts)
  #########

  # pop here
  count <- P

  # pop.overall
  count.overall <- sumP <- sum(P, na.rm=na.rm)

  # Dcount here
  count.D <- PD <- P * D
  PD[is.na(PD)] <- 0
  count.D[is.na(count.D)] <- 0

  # Dcount.overall
  count.D.overall <- sumPD <- sum(PD)

  # dcount here
  count.dref <- Pd <- P - count.D  # should be same as pct.dref * P, etc.
  Pd[is.na(Pd)] <- 0
  count.dref[is.na(count.dref)] <- 0

  # dcount.overall
  count.dref.overall <- sumPd <- sum(Pd)

  #########
  # percentage of population
  # (again, P must be proper universe)
  #########

  # Dpct here
  pct.D <- D

  # Dpct.overall  (******* this does not handle zero as denominator in zone ****)
  pct.D.overall <- sumPD / sumP

  # dpct here
  pct.dref <- 1 - pct.D

  # dpct.overall
  pct.dref.overall <- sumPd / sumP

  #########
  # cases    # or "wtdE"
  #########

  # cases here everybody      **** need to handle NA in E here... **************
  cases <- PE <- P * E
  PE[is.na(PE)] <- 0
  cases[is.na(cases)] <- 0

  # cases overall everybody
  cases.overall <- sumPE <- sum(PE)

  #(sum(cases)/1e6)/70 # if lifetime cancer risk per million is E
  #cases.overall/1e6/70 # if lifetime cancer risk per million is E

  # dref cases here
  cases.dref <- PdE <- Pd * E
  PdE[is.na(PdE)] <- 0
  cases.dref[is.na(cases.dref)] <- 0

  # dref cases overall
  cases.dref.overall <- sumPdE <- sum(PdE)

  # D cases here
  cases.D <- PDE <- PD * E
  cases.D[is.na(cases.D)] <- 0
  PDE[is.na(PDE)] <- 0
  cases.D[is.na(cases.D)] <- 0

  # D cases overall
  cases.D.overall <- sumPDE <- sum(PDE)

  ########
  # E (individual risk)
  #########

  risk <- E
  risk.D <- E
  risk.dref <- E

  risk.overall      <- sumPE  / sumP   # P-wtd.mean of E
  risk.D.overall    <- sumPDE / sumPD  # PD-wtd.mean of E
  risk.dref.overall <- sumPdE / sumPd  # Pd-wtd.mean of E  # one goal for EJ is for D overall to have this E

  risk.elsewhere      <- (sumPE  - PE)  / (sumP  - P)
  risk.D.elsewhere    <- (sumPDE - PDE) / (sumPD - PD)
  risk.dref.elsewhere <- (sumPdE - PdE) / (sumPd - Pd)    # one goal for EJ is for D here to have this E

  # riskodds <- risk / (1-risk) ???
  # riskodds.overall <- ???

  ########
  # Excess risk (has implicit goal)
  #########

  risk.excess.overall <- risk.D.overall - risk.dref.overall

  ########
  # Excess cases (has implicit goal)
  #########

  cases.excess.overall <- risk.excess.overall * count.D.overall

  ########
  # RR
  #########

  #RR.overall <- (sumPDE / sumPD) / (sumPdE / sumPd)
  RR.overall <- (risk.D.overall) / (risk.dref.overall)

  # OR
  #OR.overall <-

  ################################################
  # ALTERNATIVE SCENARIOS, AND THEN CONTRIBUTION BECAUSE NOT AT THAT SCENARIO:
  ################################################

  #  if.here.was.zero means if E was zero and also P was zero !!! i.e. If nobody lived here at all! Not just if E = 0

  egoal.v <- 0   # can be used to provide scenarios where local E is cut to zero

  ########
  # cases in other scenarios
  ########

  # THESE  WORK EVEN IF egoal.v IS A VECTOR
  cases.D.here.if.here.was.egoal <- count.D * egoal.v  #  = PD * egoal.v
  #
  cases.D.overall.if.here.was.egoal.vector        <- cases.D.overall - cases.D + (egoal.v * count.D)  #  = sumPDE - PDE + (egoal.v * PD)
  cases.dref.overall.if.here.was.egoal.vector        <- cases.dref.overall - cases.dref + (egoal.v * count.dref)

  cases.D.overall.if.here.was.risk.dref.overall   <- cases.D.overall - cases.D + (risk.dref.overall   * count.D)
  cases.dref.overall.if.here.was.risk.dref.overall   <- cases.dref.overall - cases.dref + (risk.dref.overall   * count.dref)

  cases.D.overall.if.here.was.risk.dref.elsewhere <- cases.D.overall - cases.D + (risk.dref.elsewhere * count.D)
  cases.dref.overall.if.here.was.risk.dref.elsewhere <- cases.dref.overall - cases.dref + (risk.dref.elsewhere * count.dref)

  cases.D.overall.if.here.was.zero            <- cases.D.overall - cases.D + (0 * count.D) # = sumPDE - PDE
  cases.dref.overall.if.here.was.zero          <- cases.dref.overall - cases.dref + (0 * count.dref)  # = sumPdE - PdE

  ########
  # risk in other scenarios
  ########

  risk.D.overall.if.here.was.egoal.vector    <- (cases.D.overall.if.here.was.egoal.vector / (count.D.overall) )
  risk.dref.overall.if.here.was.egoal.vector <- (cases.dref.overall.if.here.was.egoal.vector / (count.dref.overall) )

  risk.D.overall.if.here.was.risk.dref.overall    <-  (cases.D.overall.if.here.was.risk.dref.overall / (count.D.overall) )
  risk.dref.overall.if.here.was.risk.dref.overall <-  (cases.dref.overall.if.here.was.risk.dref.overall / (count.dref.overall) )

  risk.D.overall.if.here.was.risk.dref.elsewhere    <- (cases.D.overall.if.here.was.risk.dref.elsewhere / (count.D.overall) )
  risk.dref.overall.if.here.was.risk.dref.elsewhere      <- (cases.dref.overall.if.here.was.risk.dref.elsewhere / (count.dref.overall) )

  risk.D.overall.if.here.was.zero    <- (cases.D.overall.if.here.was.zero / (count.D.overall - count.D) )           # i.e. if local pop was zero
  risk.dref.overall.if.here.was.zero <- (cases.dref.overall.if.here.was.zero / (count.dref.overall - count.dref ) ) # i.e. if local pop was zero

  ########
  # RR in other scenarios   *** these use different versions of variables: but can use risk. variables above now that they exist ***
  ########

  RR.if.here.was.egoal <- (
    risk.D.overall.if.here.was.egoal.vector /
      risk.dref.overall.if.here.was.egoal.vector
  )

  RR.if.here.was.risk.dref.overall <- (
    risk.D.overall.if.here.was.risk.dref.overall   /
      risk.dref.overall.if.here.was.risk.dref.overall)

  RR.if.here.was.risk.dref.elsewhere <- (
    risk.D.overall.if.here.was.risk.dref.elsewhere /
      risk.dref.overall.if.here.was.risk.dref.elsewhere
  )

  RR.if.here.was.zero <-  (
    risk.D.overall.if.here.was.zero /
      risk.dref.overall.if.here.was.zero
  )

  RR.if.here.was.zero2 <-  (
    (sumPDE - PDE) / (sumPD - PD)) /
    ((sumPdE - PdE) / (sumPd - Pd))


  ########
  # excess risk in other scenarios
  ########
  #risk.excess.overall <- risk.D.overall - risk.dref.overall

  risk.excess.if.here.was.egoal.vector <- risk.D.overall.if.here.was.egoal.vector - risk.dref.overall
  risk.excess.if.here.was.risk.dref.overall <- risk.D.overall.if.here.was.risk.dref.overall - risk.dref.overall
  risk.excess.if.here.was.risk.dref.elsewhere <- risk.D.overall.if.here.was.risk.dref.elsewhere - risk.dref.overall
  risk.excess.if.here.was.zero <- risk.D.overall.if.here.was.zero - risk.dref.overall

  ########
  # excess cases in other scenarios  # **** need to check if these are correct and vectorize right: ****
  ########

  cases.excess.if.here.was.egoal <- risk.excess.if.here.was.egoal.vector * count.D.overall
  cases.excess.if.here.was.risk.dref.overall  <- risk.excess.if.here.was.risk.dref.overall * count.D.overall
  cases.excess.if.here.was.risk.dref.elsewhere <- risk.excess.if.here.was.risk.dref.elsewhere * count.D.overall
  cases.excess.if.here.was.zero <- risk.excess.if.here.was.zero * count.D.overall

  ################################################
  # print some results for now
  ################################################

  printstuff <- function(xname, x0,x1,x2,x3,x4){
    cat('\n',
        xname, '\n',
        x0, ' now \n')
    print(
      head(
        cbind(
          if.egoal=x1,
          if.here.was.risk.dref.overall=x2,
          if.here.was.risk.dref.elsewhere=x3,
          if.here.was.zero=x4
        ), 20
      )
    )
    print( summary(cbind(x1,x2,x3,x4)) )
  }

  xname='cases excess overall'
  x0=cases.excess.overall
  x1=cases.excess.if.here.was.egoal
  x2=cases.excess.if.here.was.risk.dref.overall
  x3=cases.excess.if.here.was.risk.dref.elsewhere
  x4=cases.excess.if.here.was.zero

  printstuff(xname,x0,x1,x2,x3,x4)

  xname='RR now'
  x0=RR.overall
  x1=RR.if.here.was.egoal
  x2=RR.if.here.was.risk.dref.overall
  x3=RR.if.here.was.risk.dref.elsewhere
  x4=RR.if.here.was.zero

  printstuff(xname,x0,x1,x2,x3,x4)
}
