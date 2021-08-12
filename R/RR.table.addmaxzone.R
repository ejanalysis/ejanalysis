#' Add to an existing RR.table the max relative risk value from any zone in the table
#'
#' @param x A table of relative risk values from RR.table()
#' @param testing If TRUE, print extra info to terminal
#'
#' @export
#'
RR.table.addmaxzone <- function(x, testing=FALSE) {

  RRS <- x

  ##################### #
  # Add max.zone slices
  ##################### #

  RRTEMP <- array(0, dim(RRS) + c(0, 0, 1))
  dimnames(RRTEMP) <- list(
    unlist(dimnames(RRS)[1]),
    unlist(dimnames(RRS)[2]),
    c('max.zone', unlist(dimnames(RRS)[3]))
  )
  if (testing) {
    print('structure of RRTEMP currently is this: '); print(str(RRTEMP))
    print('structure of RRS currently is this: '); print(str(RRS))
  }

  # *** For each D, what is the max RR for all zones? addmargins would be easier here...

  for (i in 1:length(RRS[1, , 1])) {

    if (testing) {
      cat('i = ', i, '\n')
      cat('RRS[ , i, ] is this now:\n'); print(RRS[ , i, ])
    }

    if (length(Znames) == 1) {
      # only one zone exists. max.zone is redundant, but just keep it there anyway,
      # partly to simplify merging with a multizone table.
      RRTEMP[ , i, ] <- cbind(max.zone = RRS[ , i, 1, drop = FALSE], RRS[ , i, 1, drop = FALSE])
    } else {
      # THIS USED TO SAY drop=FALSE, but rowMaxs only works if TRUE. But then might have problems if only one D?? but max.D is here, so should be OK?
      RRTEMP[ , i, ] <- cbind(
        max.zone = analyze.stuff::rowMaxs(RRS[ , i, , drop = TRUE]),
        RRS[ , i, 1:length(Znames), drop = TRUE]
      )
    }

    if (testing) {
      cat('RRTEMP[ , i, ] is this now:\n'); print(RRTEMP[ , i, ])
    }
  }
  # that warned of problem in row names duplications at some point

  RRS <- RRTEMP

  return(RRS)
}
