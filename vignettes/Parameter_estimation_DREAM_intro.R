#' 
#' ---
#' title: "Parameter estimation with the DREAM-zs algorithm"
#' output: rmarkdown::html_vignette
#' author:
#' - name: "Samuel Buis"
#' affiliation: "INRAE - EMMAH"
#' date: "`r Sys.Date()`"
#' vignette: >
#'   %\VignetteIndexEntry{Parameter estimation with DREAM-zs}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' params:
#'   eval_rmd: FALSE
#' ---
#' 
#' 
#' 
## ----setup, eval=TRUE, include=FALSE-----------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

#' 
#' 
#' ## Study Case
#' 
#' This document presents an example of use of the DREAM-zs algorithm with the Stics crop model.
#' 
#' The study case is the same as the specific and varietal parameters estimation presented in this [vignette](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html).
#' 
#' **WARNING: The interface of the DREAM method in CroptimizR is still under development. Results should be taken with care.** 
#' 
