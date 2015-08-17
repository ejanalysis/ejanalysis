#' @title Aggregate multiple columns of values by group
#' @description aggregate over zones - *** work in progress -- NOT DONE YET ***
#' @param x Dataset
#' @return by Vector defining groups
#' @param wts Weights, default is unweighted
#' @param FUN Default is weighted mean
#' @param prefix Default is 'wtd.mean.'
#' @param NA Default is TRUE
#' @examples
#' # SLOW BUT SEEMS TO WORK SOMEWHAT SO FAR
#' # 1.Do rollup of most fields as wtd mean
#'    #t2 <- rollup(bg[ , names.e], by=bg$FIPS.TRACT, wts=bg$pop)
#'    #names(t2) <- gsub('by', 'FIPS.TRACT', names(t2))
#' # 2.Do rollup of pop and areas as sum not wtd.mean: # not sure aggregate preserves sort order that rollup created, so use merge to be sure they match up on fips:
#'    #tractpop <- aggregate(bg[ , c('pop', 'area', 'sqmi', 'sqkm')], by=list(bg$FIPS.TRACT), sum)
#'    #names(tractpop) <- c('FIPS.TRACT', c('pop', 'sqmi', 'sqkm'))
#' # 3.Merge the wtd.mean fields and sum fields, sort results.
#'    #t2 <- merge(t2, tractpop, by='FIPS.TRACT')
#'    #rm(tractpop)
#'    #t2 <- t2[ order(t2$FIPS.TRACT), ]
#' #
#' # other examples:
#' # tracts   <- rollup(bg[ , names.e], by=bg$FIPS.TRACT,  wts=bg$pop); names(tracts) <- gsub('by', 'FIPS.TRACT', names(tracts))
#' # counties <- rollup(bg[ , names.e], by=bg$FIPS.COUNTY, wts=bg$pop)
#' # states   <- rollup(bg[ , names.d], by=bg$ST, wts=bg$pop)
#' # states2  <- rollup(bg[ , c('pm', 'o3')], by=bg$ST, wts=bg$pop)
#'
#' # SPECIFY WHICH FIELDS TO ROLLUP VIA WTD AVG AND WHICH TO DO VIA SUM OVER US/REGION/COUNTY/STATE/TRACT
#'
#' # source('myfunctions.R') # if not already available
#' # load('bg ... plus race eth subgrps ACS0812.RData') # if not already working with it
#'
#' # Get the wtd.mean for all except pctile, bin (or raw EJ index?)
#' #avgnames <- names(bg)
#' #avgnames <- avgnames[!(grepl('^pctile.', avgnames))]
#' #avgnames <- avgnames[!(grepl('^bin.', avgnames))]
#' #avgnames <- avgnames[!(grepl('^EJ.', avgnames))]
#'
#' # Get the sum for all the raw counts, and area
#' #sumnames <- c('area', 'pop', 'povknownratio', 'age25up', 'hhlds', 'builtunits', 'mins', 'lowinc', 'lths', 'lingiso', 'under5', 'over64', 'pre1960', 'VNI.eo', 'VNI.svi6', 'VDI.eo', 'VDI.svi6',
#' #              'hisp', 'nhaa', 'nhaiana', 'nhba',  'nhmulti', 'nhnhpia', 'nhotheralone', 'nhwa', 'nonmins', sumnames)
#' #sumnames <- c(sumnames, names(bg)[grepl('^EJ.', names(bg))] )
#' #
#' # Get the rollups of wtd.mean cols
#' #us       <- rollup( bg[ , avgnames], wts=bg$pop )
#' #regions  <- rollup( bg[ , avgnames], wts=bg$pop, by=bg$REGION)
#' #states   <- rollup( bg[ , avgnames], wts=bg$pop, by=bg$FIPS.ST)
#' #counties <- rollup( bg[ , avgnames], wts=bg$pop, by=bg$FIPS.COUNTY)
#' #tracts   <- rollup( bg[ , avgnames], wts=bg$pop, by=bg$FIPS.TRACT)
#' #
#' # Get the rollups of summed cols
#' #us <- cbind(us, rollup( bg[  ]))
#' #
#' #counties <- rollup(bg[ , c(names.e, names.d)], wts=bg$pop, by=bg$FIPS.COUNTY)
#' # doesn't work yet to use rollup??
#' # counties$pop <- rollup(bg$pop, by=bg$FIPS.COUNTY, FUN=function(x) sum(x, na.rm=TRUE))
#' #counties$pop <- aggregate(bg$pop, by=list(bg$FIPS.COUNTY), FUN=function(x) sum(x, na.rm=TRUE))
#' @export
rollup <- function( x, by, wts=NULL, FUN, prefix, na.rm=TRUE) {

  # ################################################################
  # COMPARISON OF data.table vs Hmisc summarize() for weighted means of subsets of fields
  # ################################################################
  #
  # ################################
  # using data.table
  # ################################
  #
  # require(data.table)
  # mydata = data.table(bg, key='ST')
  #
  # x= mydata[, list(
  #      pctlowinc = sum(pctlowinc * pop) / sum(pop),
  #      pctmin    = sum(pctmin    * pop) / sum(pop)
  #  ),
  #  by = "REGION"
  #  ]
  #
  ###################
  # Function below adds a data.table method (not just data.frame method) to aggregate()
  # so that when you pass a data.table to aggregate() now,
  # it will use this function, not the default aggregate():
  ###################
  #
  #     aggregate.data.table <- function(x, by, FUN=mean, ..., is.value=is.numeric) {
  #       value_columns <- names(x)[which(sapply(x, is.value))]
  #       x[,lapply(.SD,FUN,...),eval(substitute(by)),.SDcols=value_columns]
  #     }
  #
  #     bgt <- data.table(bg, key='FIPS.TRACT')
  #
  #     x=aggregate(bgt, by=list(bg$FIPS.TRACT))
  #   }
  #
  # ################################
  # using rollup() which used summarize() from Hmisc
  # ################################
  #
  # require(Hmisc)
  # #source('rollup.R')
  #
  # y= rollup(bg[,c('pctlowinc', 'pctmin')], by= bg$REGION, wts=bg$pop)
  #
  # #There were 12 warnings (use warnings() to see them)
  # ################################################################

  warning('WORK IN PROGRESS - E.G. NEED TO TEST TO VERIFY THIS CORRECTLY HANDLES NA VALUES IN FIELD AGGREGATED AND/OR WEIGHTS FIELD')
  debugging <- FALSE

  if (missing(x))  {stop('missing input: x')}
  if (missing(by)) {warning('missing parameter: by, providing one summary of all')}

  rowcount <- length(as.matrix(x)[,1])

  if (missing(FUN)) {

    wtd.mean.func <- function(y) {
      #print(str(y)); print(typeof(y)); print(dim(y)); print(length(dim(y))); print(dimnames(y)); print(is.null(dimnames(y)[1]))
      return(Hmisc::wtd.mean(y[ , 1], y[ , 2], na.rm=na.rm))
    }

    myfun <- wtd.mean.func
    if (missing(prefix)) {prefix <- 'wtd.mean.'}

  } else {
    myfun <- FUN
  }

  # if wts was specified as the name of a field in x, make sure wtscolname is set and weights is now the actual vector not just its name

  if (length(wts)==1 & class(wts)=='character') {
    wtscolname <- wts
    #weights <- x[ , wtscolname]

    # this presumes x is a data.frame, which it should be since wts was specified as name of a col in it:
    names(x) <- gsub(wtscolname, 'wxtempname', names(x)) # just use wxtempname as the colname within x

  } else {

    # if wts missing:
    if (is.null(wts)) {
      wts <- rep(1, rowcount) # simple way to do unweighted case, but would be faster to set it to NULL and have wtd.mean etc below handle that case
      wtscolname <- ifelse('no.wts.used' %in% names(x), 'no.wts.used.2', 'no.wts.used') # avoids problem if x has such a colname already
    } else {
      # if wts was specified as vector (we think)
      # We could use literally what was typed after by=    or just literally the second of the function arguments, not its value
      #  BUT THIS WOULD be a problem if wts=  was specified using a formula giving a strange name to the column in x, so used wxtempname as colname in x
      wtscolname <- ifelse('wts' %in% names(x), 'wts2', 'wts') # avoids problem if x has such a colname already
    }

    if (is.matrix(x)) {x <- as.data.frame(x)}
    if (is.data.frame(x)) {oldnames <- names(x)} else {oldnames <- 'var'}

    x <- data.frame(x, wts, stringsAsFactors=FALSE)
    names(x) <- c(oldnames, 'wxtempname') # just use wxtempname as the colname within x for now
  }

  # if by was specified as the name of a field in x, make sure by is now the actual vector not just its name
  if (length(by)==1 & class(by)=='character') {
    by <- x[ , by]
  } else {
    if (missing(by)) {
      # no "by" so just summarize all as one big group
      x$all <- factor(rep(1,length(x[,1])))
      by <- x$all
    } else {
      # literally what was typed after by=    or just literally the second of the function arguments, not its value
      #bycolname <- deparse(substitute(by)) # NOT necessarily a column name at all, though.
    }
  }

  if (debugging) {
    cat('names of x: ', names(x),'\n')
    cat('length x: ',length(x),'\n')
    cat(length(x[,1]),' rows in x\n')
    cat(rowcount,' rows in x\n')
    cat(length(unique(by)),' unique values or rows in rollup of x\n')
  }

  # preallocate memory, but not sure how this handles NAs
  rolled <- as.data.frame(matrix(nrow=length(unique(by)), ncol=length(x)), stringsAsFactors=FALSE)

  # It now is a data.frame, since it has a wts col, so do a rollup for each column other than wts,
  # but you don't really want the weights col if user didn't specify wts & their function doesn't use wts !

  if (length(dim(x)) > 1) {

    mystatnames <- paste(prefix, names(x), sep='')

    for (i in 1:length(x)) {

      # will summarize return more than one column? if so, should fix that**********
      # or just replace all this with data.table package which is so fast

      if ( !is.numeric(x[ , i])) {
        #
        # ** THIS MAY HAVE PROBLEMS RETURNING NA VALUES OFTEN, FOR SOME REASON.
        #
        # should check if numeric/ can do the FUN for this particular col? The by col or any nonnumeric col gets summarized as 1st instance of it per by group.
        # to roll up the by column, just use first value of by in each group (OR TRY USING unique(by) BUT NOT SURE OF SORT ORDER)

        # While debugging, print names of fields as they are summarized:
        cat(analyze.stuff::lead.zeroes(i,3), '- using 1st element per subset for non numeric field: ', names(x)[i], '\n')

        rolled[ , i] <- as.vector(summarize(x[ , i], by=llist(by), FUN=function(y) y[1]) )[ , 2]

      } else {
        # ************** if don't want wtd.mean, and don't need wts, this is not ideal: want to be able to write function of a vector, not necessarily a data.frame!
        # This seems to assume we want wtd.mean since it passes var and weight

        if (debugging) {
          cat(analyze.stuff::lead.zeroes(i,3), '- summarizing ', names(x)[i], '\n')
        }

        if (missing(FUN)) {
          rolled[ , i] <- (summarize(x[ , c(names(x)[i], 'wxtempname')], by=llist(by), FUN=myfun))[ , 2]
        } else {
          # THIS ASSUMES A USER DEFINED FUNCTION DOES NOT USE THE WEIGHTS PARAMETER
          rolled[ , i] <- (summarize(x[ , names(x)[i] ],         by=llist(by), FUN=myfun))[ , 2]
        }
      }
      # x[ , i], match('wxtempname', names(x))  instead of names(x)[i], 'wxtempname')  ??
    }

    # assemble those cols of rolled correctly?

  } else {
    cat('you should not be here!\n')
    if (debugging) { cat('names(x)[i]: ','names(x)[i]','\n') }
    rolled <- summarize(x[ , c(names(x)[i], 'wxtempname')], by=llist(by), FUN=myfun)
    #names(rolled) <- c(names(by), )
  }

  if (debugging) {
    # While debugging, print names of fields as they are summarized:
    cat('mystatnames is ', mystatnames,'\n')
    cat('names of rolled so far are: ', names(rolled), '\n')
  }

  names(rolled) <- mystatnames
  names(rolled) <- gsub(paste(prefix, 'wxtempname', sep=''), wtscolname, names(rolled))

  if (debugging) {

    cat('names of rolled now are: ', names(rolled), '\n')
    cat('names of x are:', names(x),'\n')
    cat('wtscolname is :',wtscolname,'\n')
  }

  # Actually, a sum of the weights is probably more useful than the weighted mean of the weights!
  rolled[ , wtscolname] <- (summarize(x[ , 'wxtempname'], by=llist(by), FUN=function(y) sum(y, na.rm=TRUE)))[ , 2]

  if (debugging) {
    # While debugging, print names of fields as they are summarized:
    cat('names of rolled now', names(rolled),'\n')
  }

  # include the "by" and wts fields as the first two columns returned
  rolled$by <- as.vector(summarize(by, by=llist(by), FUN=function(y) y[1]) )

  if (debugging) {
    # While debugging, print
    cat('names of rolled now after added by col:', names(rolled),'\n')
    cat(' wtscolname: ', wtscolname, '\n')
    cat('names of rolled I tried to subset on: ', c('by', wtscolname, names(rolled[!(names(rolled) %in% c('by', wtscolname) )])),'\n')
  }

  rolled <- rolled[ , c('by', wtscolname, names(rolled[!(names(rolled) %in% c('by', wtscolname) )]))]

  # it is the sum of the weights, so name that column to say so
  names(rolled) <- gsub(wtscolname, paste('sum.',wtscolname,sep=''), names(rolled) )

  return(rolled)

}
