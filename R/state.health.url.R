#' @title Get URL(s) with State health indicator data from RWJF - ** url scheme obsolete now so needs to be redone
#' @description Robert Woods Johnson Foundation provides health indicator data by state. This function provides a basic interface to those webpages.
#' @details 
#' NEWER URL SCHEME NOW... E.G. \url{http://www.rwjf.org/en/how-we-work/rel/research-features/rwjf-datahub/national.html#q/scope/national/ind/10/dist/0/char/0/time/3/viz/map/cmp/brkdwn}
#' 
#' see also related percent insured data here: e.g., 
#' \url{http://datacenter.shadac.org/profile/70#2/alabama/percent,moe,count/a/hide}
#' DEFAULT IF NO PARAMETERS USED:
#' state.health.url() \cr
#' url created: \cr
#' http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/87/time/3/viz/bar/fstate/33/locs/33,52,9/cmp/stcmp \cr
#' url that it resolves to on website: \cr
#' http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/87/time/3/viz/bar/fstate/33/locs/33,52,9/cmp/stcmp \cr
#' # state.health.url("MD") \cr
#' # url created: \cr
#' # \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/87/time/14/viz/bar/fstate/21/locs/21/cmp/stcmp} \cr
#' # url that it resolves to on website should be  \cr
#' # \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/87/time/3/viz/bar/fstate/21/locs/21,52/cmp/stcmp} \cr
#' #	URL scheme for linking to Resources for health data by state or by county: \cr
#' \cr
#' #State-level data from SHADAC: \url{http://www.shadac.org/} \cr
#' #State-level data from RWJF: \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub.html} \cr
#' ***** see entire US clickable map of states
#' # shell.exec('http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/national/ind/31/dist/29/char/119/time/13/viz/map/fstate/2/locs/2,52/cmp/brkdwn')
#' # Use this URL format to figure out API or state-specific set of links to nice state reports. 
#' Public health, premature deaths, MD vs US, 2009-2010 
#' \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/91/time/3/viz/bar/fstate/21/locs/21,52/cmp/stcmp}
#' Public health, life expectancy, MD (state number 21) vs US
#' \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/31/dist/29/char/119/time/14/viz/line/fstate/21/locs/21,52/cmp/brkdwn}
#' \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/31/dist/29/char/119/time/14/viz/bar/fstate/21/locs/21,52/cmp/stcmp} \cr
#' Cancer by race in arizona (defaults to 2008-2009): 
#' \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/7/dist/22/char/85/time/18/viz/bar/fstate/3/locs/3,52/cmp/stcmp}
#' @param ST State abbreviation, FIPS code, or name as character vector
#' @param scope Character, default is "state" view in results, but can be "national" view as well
#' @param ind Health Indicator code number. Number, default is 66. Possible values: \cr
#' \itemize{
#' \item 6=Cancer Incidence: Incidence of breast, cervical, lung and colorectal cancer per 100,000 population; age adjusted
#' \item 7=Cancer Incidence by Race: Incidence of breast, cervical, lung and colorectal cancer per 100,000 population; age adjusted
#' \item 10=Chronic Disease Prevalence: asthma/CVD/diabetes, 
#' \item 15=Limited Activity: Average number of days in the previous 30 days when a person indicates their activities are limited due to mental or physical health difficulties, 
#' \item 22=Tobacco taxes: State cigarette excise tax rate ($)
#' \item 25=Income Inequality (Gini Coefficient)
#' \item 31=life expectancy= Life expectancy at birth: number of years that a newborn is expected to live if current mortality rates continue to apply
#' \item 44=Public health funding: Per capita state public health funding
#' \item 45=Poor/Fair Health: Self-reported health status: percent of adults reporting fair or poor health
#' \item 65=Premature death: Premature deaths: Average number of years of potential life lost prior to age 75 per 100,000 population
#' \item 66=Premature Death by Race/Ethnicity: Premature deaths: Average number of years of potential life lost prior to age 75 per 100,000 population
#' }
#' @param dist default is 23. unclear what this controls. 23 or 29 or 0 or 19 possible. dist/char were 0/0 for US totals not by race, and were 19/58 for same by race.
#' @param char default is 87. unclear what this controls. 91=?, 119=?, 121=?  58? others possible.
#' @param time Code for which years are covered by the data. default is 3, which means 2009-2010. Possible values: \cr
#' \itemize{
#' \item 3=2009-2010
#' \item 22=2010-2011
#' \item 23=2011-2012 
#' \item 5=2000
#' \item 10=2005 
#' \item 11=2006 
#' \item 12=2007 
#' \item 13=2008 
#' \item 1=2009 
#' \item 14=2010  
#' \item 4=2011 
#' \item 24=2012
#' \item OTHERS? 
#' }
#' @param viz Type of visualization of data. Default is "bar" and can be line or bar or table.
#' @param fstate State number. default is NA
#' @param locs Locations to compare. default is NA
#' @param cmp Comparisons to view. Default is "stcmp" to see 2+ states compared (or state vs US). Can also see breakdown for one state if set to "brkdwn"
#' @param open.browser Logical, default is FALSE. Should URL be opened by launching browser.
#' @return URL(s) character vector, default: \url{http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q/scope/state/ind/66/dist/23/char/87/time/3/viz/bar/fstate/33/locs/33,52,9/cmp/stcmpind/66/dist/23/char/87/time/3/viz/bar/fstate/33/locs/33,52,9/cmp/stcmp}
#' @examples #
#'  shell.exec(state.health.url('CA'))
#'
#' @export 
state.health.url <- function(ST=NA, scope="state", ind=66, dist=23, char=87, time=3, viz="bar", fstate=NA, locs=NA, cmp="stcmp", open.browser=FALSE) {

  # Requires list of state numbers corresponding to state abbreviations so user can use state abbreviation as parameter.
  # Get table of states with numbering by alpha of full name, which is how RWJF site numbers them in URLs
  #x<-data.frame(state.name,state.abb,stringsAsFactors=FALSE)
  #x<-rbind(x,c("District of Columbia","DC"))
  #x<-x[order(x$state.name),]
  #x<-data.frame(x,fstate=1:51)
  #rownames(x)<-1:51
  #x[52,]<-c("United States","US",52)
  #dput(x)
  
  stnums <- structure(list(state.name = c("Alabama", "Alaska", "Arizona", 
  "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
  "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
  "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
  "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
  "Wyoming", "United States"), state.abb = c("AL", "AK", "AZ", 
  "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
  "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
  "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
  "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", 
  "WA", "WV", "WI", "WY", "US"), fstate = c("1", "2", "3", "4", 
  "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", 
  "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
  "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", 
  "49", "50", "51", "52")), .Names = c("state.name", "state.abb", 
  "fstate"), row.names = c(NA, 52L), class = "data.frame")

  #########
  # basic error checking of parameters

  # specify the state using fstate, but set it based on ST if that was specified (& ignore fstate specified). 
  if (!is.na(ST)) {
  	# default to US MAP if bad ST state abbreviation was passed to function:
  	if (!(ST %in% stnums$state.abb)) {scope <- 'national'; fstate=52} else {
  	  fstate <- stnums$fstate[ stnums$state.abb==ST]
  	} 
  } else {
 	  if (!(fstate %in% c(1:52))) {scope <- 'state'; ind<-66; fstate<-33; time<-3; char<-87; dist<-23;locs<-"33,52,9" } # Default to certain MAP if ST not specified and fstate was mis-specified, or if both are unspecified.
  }

  if (!(scope %in% c("state", "national"))) {scope <- "state"; fstate=33} # if specified incorrectly

  if (!(ind %in% c(6,7,10,15,22,25,31,44,45,65,66  ))) {ind <- 66} # Default to premature deaths by race/ethnicity. Only allows public health category (& inc inequality).
  if (!(dist %in% c(23, 29        ))) {dist <- 23} # ******************* not clear what this is
  if (!(char %in% c(91, 121, 119          ))) {char <- 87} # *******************not clear what this is
  if (!(time %in% c(  1,3:9,11:14,23:24               ))) {time <- 14} # ******************* not clear what all the options are and how to set a good default

  if (!(viz %in% c("bar", "line", "table" ))) {viz <- "bar"}  # not always possible

  # ensure this is a list of numbers 1-52 separated by commas, possibly with spaces; OK if number not character when just 1 #; fails to catch trailing comma or period but OK
  if (any(!(as.numeric(unlist(strsplit(gsub(" ", "", locs),","))) %in% 1:52)))  {locs <- fstate} 


  if (!(cmp %in% c('stcmp', 'brkdwn'))) { locs <- fstate }

  # if no locs parameter specified so far, it is set to either just this state or this state vs. US overall
  if (is.na(locs) | nchar(locs)<3) {
  	if (cmp=='stcmp') { locs <- paste(fstate,"52",sep=",") }
  	if (cmp=='brkdwn') { locs <- fstate }
  }  

  # new scheme:
  # http://www.rwjf.org/en/how-we-work/rel/research-features/rwjf-datahub/national.html#q/scope/national/ind/10/dist/0/char/0/time/3/viz/map/cmp/brkdwn
  
  #my.url <- paste("http://www.rwjf.org/en/research-publications/research-features/rwjf-datahub/national.html#q",
  my.url <- paste("http://www.rwjf.org/en/how-we-work/rel/research-features/rwjf-datahub/national.html#q",
	"/scope/",	scope,	# scope is one "state", typically, but can see "national" too
	"/ind/", 	ind, 
	"/dist/", 	dist,
	"/char/", 	char,
	"/time/", 	time,
	"/viz/", 	viz,
	"/fstate/", 	fstate,
	"/locs/", 	locs,
	"/cmp/", 	cmp,
	sep="")

  if (open.browser) {
    shell.exec(my.url)
  }
  
  return(my.url)
}

#############################

# LIFE EXPECTANCY DATASET FOR ONE STATE AS DOWNLOADED AS CSV:
#Life expectancy at birth: number of years that a newborn is expected to live if current mortality rates continue to apply					
#Location	Race/Ethnicity	TimeFrame	DataFormat	Data	SE
#Alabama	African-American/Black	2000	Number	71.1	
#Alabama	African-American/Black	2005	Number	71.3	
#Alabama	African-American/Black	2008	Number	72.1	
#Alabama	African-American/Black	2010	Number	72.9	
#Alabama	Total	2000	Number	74.6	
#Alabama	Total	2005	Number	74.6	
#Alabama	Total	2008	Number	75	
#Alabama	Total	2010	Number	75.4	
#Alabama	Asian	2000	Number	N/A	
#Alabama	Asian	2005	Number	N/A	
#Alabama	Asian	2008	Number	N/A	
#Alabama	Asian	2010	Number	85.3	
#Alabama	Hispanic/Latino	2000	Number	N/A	
#Alabama	Hispanic/Latino	2005	Number	N/A	
#Alabama	Hispanic/Latino	2008	Number	N/A	
#Alabama	Hispanic/Latino	2010	Number	N/A	
#Alabama	American Indian/Alaskan Native	2000	Number	N/A	
#Alabama	American Indian/Alaskan Native	2005	Number	N/A	
#Alabama	American Indian/Alaskan Native	2008	Number	N/A	
#Alabama	American Indian/Alaskan Native	2010	Number	N/A	
#Alabama	Non-Hispanic White	2000	Number	75.5	
#Alabama	Non-Hispanic White	2005	Number	75.5	
#Alabama	Non-Hispanic White	2008	Number	75.7	
#Alabama	Non-Hispanic White	2010	Number	76	
