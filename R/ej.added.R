#' @title Contribution of each place to net excess people-points in demographic group
#' @description US total sum of net excess vulnerable * E = net excess people-points
#' and contribution of one place = what the EJ Index intends to indicate
#'
#' @details
#' WORK IN PROGRESS. ! e.g.,  presumes one setting for vs..... ***
#' Directly calculate total number of excess people-points in a demographic subgroup,
#' across all locations
#' **** here by default defining "excess" as above
#' what it would be if e in d group were same as e in nond group.**** \cr
#' where people-points are e * p * d \cr
#' e = environmental points or individual risk (vector of places) \cr
#' p = population counts (vector of places) \cr
#' d = demographic fraction that is in specified demographic group (vector of places) \cr \cr
#'
#' For example, if e=cancer risk (individual risk) and p=pop and d=%lowincome,
#' value returned is number of cases among lowincome individuals in excess of what it
#' would be if their average risk was the same as that of nonlowincome individuals. \cr
#' net excess vulnerable * E = net excess people-points \cr
#' vs can be 'avg' or 'nonD' (default) (not case sensitive),  \cr
#' for, respectively, excess cases relative to risk scenario where  \cr
#' avg d's e is set to that of avg person (all people including d and nonD), \cr
#' or avg d's e is set to that of avg nonD person.
#' @param e Environmental indicator
#' @param d Demographic indicator
#' @param p Population count (universe for d)
#' @param vs Reference group, optional, 'nond' by default
#'   which means the reference group is everyone other than the d group (everyone else),
#'   but can also specify 'avg' in which case the overall average value
#'   including the d group is used as the reference value.
#' @param silent Optional, TRUE by default, in which case more details are printed.
#' @return Returns numeric vector
#' @examples
#' x=data.frame(pop=rep(1000, 10), pct=0.05+6 * (1:10)/100, e= (10 * (1:10))/100 )
#' y=ej.added(x$e, x$pct, x$pop, silent=FALSE)
#' @export
ej.added <- function(e, d, p, vs='nond', silent=TRUE) {

  if (!(tolower(vs) %in% c('avg', 'nond'))) {stop('vs must be avg or nonD')}

  valid <- !(is.na(e) | is.na(p) | is.na(d))
  if (any(!valid)) {warning(paste(length(e) - sum(valid), 'inputs are NA (for e,p,d,or some combination of them) and NA is returned for those & they are excluded from calculations'))}
  e <- e[valid]
  p <- p[valid]
  d <- d[valid]

  v <- p * d
  nonv <- p * (1 - d)

  # for cancer, divide these cases by 7e7 to get annual cases actually, since per million per 70 year lifetime)
  v.cases <- e * v
  nonv.cases <- e * nonv

  # US TOTALS OR POP MEANS
  pop.US <- sum(p, na.rm = TRUE)
  VSI.eo.US <- sum(v, na.rm = TRUE) / sum(p, na.rm = TRUE) # (treats d as % of all people, not just denominator that has only those for whom poverty status determined)
  v.cases.US <- sum(v.cases, na.rm = TRUE)
  nonv.cases.US <- sum(nonv.cases, na.rm = TRUE)

  nonv.e.US <- Hmisc::wtd.mean(e, weights = nonv, na.rm = TRUE)
  v.e.US <-    Hmisc::wtd.mean(e, weights = v, na.rm = TRUE)
  all.e.US <-  Hmisc::wtd.mean(e, weights = p, na.rm = TRUE)
  #nonv.e.US <- nonv.cases.US / sum(nonv, na.rm = TRUE) # wtd avg of e, wtd by count of nonv
  #v.e.US <-       v.cases.US / sum(v,    na.rm = TRUE)
  #all.e.US <-  (v.cases.US + nonv.cases.US) / sum(v, na.rm = TRUE)

  # EJ INDEX
  ej.index <- e * p * (d - VSI.eo.US)  # EJ INDEX AS IN screening for 2014, appears to be vs avg e, not vs nonD's e.
  ej.index.vs.nond <- ej.index + (v.cases * VSI.eo.US) # DERIVED FORMULA 12/23/2014, should be vs nonD's e.
  ej.index.US = sum(ej.index)
  ej.index.vs.nond.US <- sum(ej.index.vs.nond)

  # USE SELECTED REFERENCE GROUP - vs avg or nond:
  if (tolower(vs) == 'avg') {
    v.e.if.no.ej <- all.e.US # just for the v people, suppose every location's e were same as the (original) US avg, which would be higher than the nonV pop's avg., assuming v's e is > nonv's e overall.
    excess.e.US <- v.e.US - all.e.US
    rr1 <- v.e.US / all.e.US
  }

  if (tolower(vs) == 'nond') {
    v.e.if.no.ej <- nonv.e.US # just for the v people, suppose every location's e were same as the nonv pop's US avg.
    excess.e.US <- v.e.US - nonv.e.US
    # CAN DIRECTLY CALCULATE V CASES IF THEY HAD THE SAME NUMBER OF CASES AS NONV, BUT SCALED TO REFLECT THEIR SHARE OF POPULATION.
    # THIS IS THE SAME AS SAYING THEIR E, OR RISK, OR CASES PER PERSON, IS THE SAME AS THAT OF NONV.
    #
    # THIS MAY BE SLIGHTLY MISLEADING OR UNREALISTIC SINCE IT ASSUMES NONV RISK IS AS-IS UNCHANGED
    # WHILE V RISK IS SOMEHOW REDUCED:

    # DEFINED BUT NOT USED
    v.cases.if.no.ej.US2 <- (VSI.eo.US / (1 - VSI.eo.US)) * (nonv.cases.US)
    # DEFINED BUT NOT USED
    v.cases.if.no.ej.US3 <- (sum(v, na.rm = TRUE)/sum(nonv, na.rm = TRUE)) * (nonv.cases.US)   # is another way to get this
    rr1 <- v.e.US / nonv.e.US
  }

  # US OVERALL
  v.cases.if.no.ej.US <- sum(v.e.if.no.ej * v, na.rm = TRUE)
  excess.cases.US <- v.cases.US - v.cases.if.no.ej.US
  rr2 <- v.cases.US / v.cases.if.no.ej.US


  ############# #
  # We found the US totals, but now figure out manually the contribution of each place to that...
  # Calculate vector of US totals, where nth is the total if nth place has e set to 0.
  # For each of these variables:
  ############# #

  nonv.cases.US.ex0 <- nonv.cases.US - nonv.cases # a vector of US totals, where each is for place x having e=0
  v.cases.if.ex0.US <- v.cases.US - v.cases # a vector of US totals, where each is for place x having e=0
  p.0ifna <- ifelse(is.na(p), 0, p)
  v.0ifna <- ifelse(is.na(v), 0, v)
  VSI.eo.US.ex0 <- (sum(v,na.rm = TRUE) - v.0ifna) / (sum(p,na.rm = TRUE) - p.0ifna)  # a vector of US totals


  # **** but this presumes one setting for vs..... ******
  # **** but this presumes one setting for vs..... ******
  # **** but this presumes one setting for vs..... ******
  v.cases.if.no.ej.and.ex0.US <- (VSI.eo.US.ex0 / (1 - VSI.eo.US.ex0)) * nonv.cases.US.ex0  # a vector of US totals
  # **** but this presumes one setting for vs..... ******
  # **** but this presumes one setting for vs..... ******
  # **** but this presumes one setting for vs..... ******

  excess.cases.US.if.ex0 <- v.cases.if.ex0.US - v.cases.if.no.ej.and.ex0.US # a vector of US totals

  excess.cases.added <- excess.cases.US - excess.cases.US.if.ex0 # a vector of US totals

  # PUT BACK IN THE PLACES THAT HAD NA VALUES, TO RETURN A VECTOR THAT MATCHES THE INPUT VECTOR:
  excess.cases.added.NA.included <- valid
  excess.cases.added.NA.included[valid] <- excess.cases.added
  excess.cases.added.NA.included[!valid] <- NA

  if (silent == FALSE) {
    cat('vs=',vs,'\n')
    cat('\n')
    print( round(t(cbind(VSI.eo.US,
                pop.US, v.e.US, nonv.e.US, all.e.US, excess.e.US, rr1, rr2,
                nonv.cases.US, v.cases.US, v.cases.if.no.ej.US, excess.cases.US, ej.index.US, ej.index.vs.nond.US
                 ) ), 2))
    cat('\n')
    print( head(cbind(e = e,
      p = p, d = d,v = v, VSI.eo.US.ex0,
      v.cases, v.cases.if.ex0.US, v.cases.US, velsewhere = v.cases.US - v.cases, ej.index, ej.index.vs.nond,
      v.cases.if.no.ej.and.ex0.US, nonv.cases.US.ex0, excess.cases.US.if.ex0,
      contrib = v.cases.if.no.ej.US - v.cases.if.no.ej.and.ex0.US, excess.cases.added), 10 ) )
    cat('\n')
    cat('excess.cases.added.NA.included: \n'); print(head(excess.cases.added.NA.included, 10))
  }

  return(excess.cases.added.NA.included)

  if (1 == 0) {

    # examples:

    mydat <- data.frame(traffic.score = rnorm(n = 100, mean = 500, sd = 100), VSI.eo = runif(n = 100, min = 0, max = 1), pop = 1000)
    e <- mydat$traffic.score
    p <- mydat$pop
    d <- mydat$VSI.eo
    # Must specify names.e, names.ej here also for example to work

    for (i in 1:12) {print(excess.cases(mydat[ , names.e[i]],  d, p) / sum(p * d * mydat[,names.e[i]], na.rm = TRUE))}
    mydatv <- mydat[ !is.na(rowSums(mydat[,names.e])), c('pop','VSI.eo',names.e)]
    for (i in 1:12) {print(excess.cases(mydatv[, names.e[i]],  mydatv$VSI.eo, mydatv$pop) / sum(mydatv$pop * mydatv$VSI.eo * mydatv[,names.e[i]],na.rm = TRUE ) )}
    for (i in 1:12) {print(RR(mydatv[, names.e[i]], mydatv$VSI.eo, mydatv$pop)   )}

    ### #

    plotd <- function(n) {
      x = ej.added(mydat[,names.e[n] ], mydat$VSI.eo,mydat$pop)
      # These differ but shouldn't really differ???
      lims = stats::quantile(x,probs = c(0.01,0.99),na.rm = TRUE)
      plot(x, mydat[,names.ej[n]], pch='.', main='comparison of manual calc of excess cases added to EJ Index', xlab='manually calculated contribution of mydat to excess cases among v', ylab='EJ Index', xlim=lims, ylim=lims)
      abline(h = stats::quantile(mydat[,names.ej[n]],probs = c(0.8,.9,.95),na.rm = TRUE))
      abline(v = stats::quantile(x,probs = c(0.8,.9,.95),na.rm = TRUE))
    }
    plotd(2)

    ### #

        #   > # example
    #     > x=data.frame(pop=rep(1000, 10), pct=0.05+6*(1:10)/100, e= (10*(1:10))/100 )
    #   > y=ej.added(x$e, x$pct, x$pop, silent=FALSE, vs='nond')
    #   vs= nond
    #
    #   [,1]
    #   VSI.eo.US               0.38
    #   pop.US              10000.00
    #   v.e.US                  0.68
    #   nonv.e.US               0.47
    #   all.e.US                0.55
    #   excess.e.US             0.21
    #   rr1                     1.45
    #   rr2                     1.45
    #   nonv.cases.US        2915.00
    #   v.cases.US           2585.00
    #   v.cases.if.no.ej.US  1786.61
    #   excess.cases.US       798.39
    #   ej.index.US           495.00
    #   ej.index.vs.nond.US  1477.30
    #
    #   e    p    d   v VSI.eo.US.ex0 v.cases v.cases.if.ex0.US v.cases.US velsewhere ej.index ej.index.vs.nond v.cases.if.no.ej.and.ex0.US
    #   [1,] 0.1 1000 0.11 110     0.4100000      11              2574       2585       2574      -27           -22.82                    1963.831
    #   [2,] 0.2 1000 0.17 170     0.4033333      34              2551       2585       2551      -42           -29.08                    1858.263
    #   [3,] 0.3 1000 0.23 230     0.3966667      69              2516       2585       2516      -45           -18.78                    1764.619
    #   [4,] 0.4 1000 0.29 290     0.3900000     116              2469       2585       2469      -36             8.08                    1682.115
    #   [5,] 0.5 1000 0.35 350     0.3833333     175              2410       2585       2410      -15            51.50                    1610.000
    #   [6,] 0.6 1000 0.41 410     0.3766667     246              2339       2585       2339       18           111.48                    1547.556
    #   [7,] 0.7 1000 0.47 470     0.3700000     329              2256       2585       2256       63           188.02                    1494.095
    #   [8,] 0.8 1000 0.53 530     0.3633333     424              2161       2585       2161      120           281.12                    1448.958
    #   [9,] 0.9 1000 0.59 590     0.3566667     531              2054       2585       2054      189           390.78                    1411.513
    #   [10,] 1.0 1000 0.65 650     0.3500000     650              1935       2585       1935      270           517.00                    1381.154
    #   nonv.cases.US.ex0 excess.cases.US.if.ex0    contrib excess.cases.added
    #   [1,]              2826               610.1695 -177.21761         188.217605
    #   [2,]              2749               692.7374  -71.64967         105.649667
    #   [3,]              2684               751.3812   21.99412          47.005881
    #   [4,]              2631               786.8852  104.49815          11.501851
    #   [5,]              2590               800.0000  176.61290          -1.612903
    #   [6,]              2561               791.4439  239.05675           6.943247
    #   [7,]              2544               761.9048  292.51767          36.482335
    #   [8,]              2539               712.0419  337.65479          86.345212
    #   [9,]              2546               642.4870  375.09995         155.900050
    #   [10,]              2565               553.8462  405.45906         244.540943
    #
    #   excess.cases.added.NA.included:
    #     [1] 188.217605 105.649667  47.005881  11.501851  -1.612903   6.943247  36.482335  86.345212 155.900050 244.540943



    #     > # example
    #     > x=data.frame(pop=rep(1000, 10), pct=0.05+6*(1:10)/100, e= (10*(1:10))/100 )
    #   > y=ej.added(x$e, x$pct, x$pop, silent=FALSE, vs='avg')
    #   vs= avg
    #
    #   [,1]
    #   VSI.eo.US               0.38
    #   pop.US              10000.00
    #   v.e.US                  0.68
    #   nonv.e.US               0.47
    #   all.e.US                0.55
    #   excess.e.US             0.13
    #   rr1                     1.24
    #   rr2                     1.24
    #   nonv.cases.US        2915.00
    #   v.cases.US           2585.00
    #   v.cases.if.no.ej.US  2090.00
    #   excess.cases.US       495.00
    #   ej.index.US           495.00
    #   ej.index.vs.nond.US  1477.30
    #
    #   e    p    d   v VSI.eo.US.ex0 v.cases v.cases.if.ex0.US v.cases.US velsewhere ej.index ej.index.vs.nond v.cases.if.no.ej.and.ex0.US
    #   [1,] 0.1 1000 0.11 110     0.4100000      11              2574       2585       2574      -27           -22.82                    1963.831
    #   [2,] 0.2 1000 0.17 170     0.4033333      34              2551       2585       2551      -42           -29.08                    1858.263
    #   [3,] 0.3 1000 0.23 230     0.3966667      69              2516       2585       2516      -45           -18.78                    1764.619
    #   [4,] 0.4 1000 0.29 290     0.3900000     116              2469       2585       2469      -36             8.08                    1682.115
    #   [5,] 0.5 1000 0.35 350     0.3833333     175              2410       2585       2410      -15            51.50                    1610.000
    #   [6,] 0.6 1000 0.41 410     0.3766667     246              2339       2585       2339       18           111.48                    1547.556
    #   [7,] 0.7 1000 0.47 470     0.3700000     329              2256       2585       2256       63           188.02                    1494.095
    #   [8,] 0.8 1000 0.53 530     0.3633333     424              2161       2585       2161      120           281.12                    1448.958
    #   [9,] 0.9 1000 0.59 590     0.3566667     531              2054       2585       2054      189           390.78                    1411.513
    #   [10,] 1.0 1000 0.65 650     0.3500000     650              1935       2585       1935      270           517.00                    1381.154
    #   nonv.cases.US.ex0 excess.cases.US.if.ex0  contrib excess.cases.added
    #   [1,]              2826               610.1695 126.1695         -115.16949
    #   [2,]              2749               692.7374 231.7374         -197.73743
    #   [3,]              2684               751.3812 325.3812         -256.38122
    #   [4,]              2631               786.8852 407.8852         -291.88525
    #   [5,]              2590               800.0000 480.0000         -305.00000
    #   [6,]              2561               791.4439 542.4439         -296.44385
    #   [7,]              2544               761.9048 595.9048         -266.90476
    #   [8,]              2539               712.0419 641.0419         -217.04188
    #   [9,]              2546               642.4870 678.4870         -147.48705
    #   [10,]              2565               553.8462 708.8462          -58.84615
    #
    #   excess.cases.added.NA.included:
    #     [1] -115.16949 -197.73743 -256.38122 -291.88525 -305.00000 -296.44385 -266.90476 -217.04188 -147.48705  -58.84615  #plot(excess.cases.added, mydat[,names.ej[3]],pch='.')


  }
}

