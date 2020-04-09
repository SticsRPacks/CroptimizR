#' ---
#' title: "Parameter estimation with the ApsimX crop Model: a simple case"
#' output: rmarkdown::html_vignette
#' author: 
#' - name: "Patrice Lecharpentier"
#'   affiliation: "INRAE - Agroclim"
#' - name: "Samuel Buis"
#'   affiliation: "INRAE - EMMAH"
#' - name: "Drew Holzworth"
#' date: "`r Sys.Date()`"
#' vignette: >
#'   %\VignetteIndexEntry{Parameter estimation with ApsimX}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' params:
#'   eval_rmd: FALSE
#' ---
#' 
#'   
## ----setup, eval=TRUE, include=FALSE--------------------------------------------------------------------------------------------------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

#' 
#' ## Study Case
#' 
#' A simple parameter estimation with a single situation, a single observed variable and 2 estimated parameters, just to illustrate how to use the package with the ApsimX model.
#' 
#' The parameter estimation is performed using the Nelder-Meade simplex method implemented in the `nloptr` package.
#' 
#' 
#' ## Initialisation step
#' 
## ----setup_initializations, message=FALSE, results=FALSE, warning=FALSE---------------------------------------------------------------------------------------

# Install and load the needed libraries
if(!require("CroptimizR")){
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
if(!require("ApsimOnR")){
  devtools::install_github("ApsimOnR")
  library("ApsimOnR")
}
if(!require("dplyr")){
  install.packages("dplyr",repos="http://cran.irsn.fr")
  library("dplyr")
}
