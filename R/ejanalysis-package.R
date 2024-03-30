#' @title Tools for Environmental Justice (EJ) Analysis
#' @name ejanalysis
#' @aliases ejanalysis-package
#' @concept justice EJ demographic
#' @description This R package provides tools for environmental justice (EJ) analysis,
#' including metrics characterizing disparities between demographic groups,
#' such as differences in environmental or other indicators measured for each Census unit
#' such as Census blocks, block groups, tracts, zip codes, or counties.
#'
#' These tools simplify some basic tasks in
#' exploring and analyzing a dataset in a matrix or data.frame
#' that contains data on demographics (e.g., counts of residents in poverty)
#' and local environmental indicators (e.g., an air quality index),
#' with one row per spatial location (e.g., Census block group).
#' Key functions help to find relative risk or similar ratios of means in demographic groups,
#' , etc.
#' @details
#' Key functions include
#'
#' - [RR()]
#' - [pop.ecdf()]
#' - [ej.indexes()]
#' - [assign.pctiles()]
#' - [make.bin.pctile.cols()]
#' - [get.county.info()]
#' - [state.health.url()]
#'
#' @references
#' - More information on the package is available at
#' <http://ejanalysis.github.io> \cr
#' <http://www.ejanalysis.com/> \cr
"_PACKAGE"
NULL
