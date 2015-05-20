#' @docType package
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
#' \cr
#' \itemize{
#' \item \code{\link{RR}}
#' \item \code{\link{pop.ecdf}}
#' \item \code{\link{ej.indexes}}
#' \item \code{\link{assign.pctiles}}
#' \item \code{\link{make.bin.pctile.cols}}
#' \item \code{\link{get.county.info}}
#' \item \code{\link{state.health.url}}
#' }
#'
NULL
