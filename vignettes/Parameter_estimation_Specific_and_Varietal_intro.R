#' ---
#' title: "Parameter estimation with the Stics crop Model: a case with specific and varietal parameters"
#' output: rmarkdown::html_vignette
#' author:
#' - name: "Samuel Buis"
#' affiliation: "INRA - EMMAH"
#' date: "`r Sys.Date()`"
#' vignette: >
#'   %\VignetteIndexEntry{Parameter estimation with Stics}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' params:
#'   eval_rmd: FALSE
#' ---
#' 
## ----setup, eval=TRUE, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

#' 
#' ## Study Case
#' 
#' This document presents an example of a simultaneous estimation of one specific and one varietal parameter on a multi-varietal dataset using the Stics model, while a simpler introductory example is presented in this [vignette](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html) (you should look at it first).
#' 
#' Data comes from a maize crop experiment (see description in Wallach et al., 2011). In this example, 8 situations (USMs in Stics language) will be used for the parameter estimation. This test case correspond to case 1c in (Wallach et al., 2011).
#' 
#' The parameter estimation is performed using the Nelder-Meade simplex method implemented in the nloptr package.
#' 
#' 
