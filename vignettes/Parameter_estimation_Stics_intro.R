#' ---
#' title: "Parameter estimation with the Stics crop Model: a simple case"
#' output:
#'   rmarkdown::html_vignette
#' author:
#' - name: "Samuel Buis"
#' affiliation: "INRA - EMMAH"
#' date: "`r Sys.Date()`"
#' vignette: >
#'   %\VignetteIndexEntry{Vignette Title}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' params:
#'   eval_rmd: FALSE
#' ---
#'
## ----setup, eval=TRUE, include=FALSE------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

#'
#' ## Study Case
#'
#' This document presents an example of a simple parameter estimation using the Stics model with a single situation, a single observed variable and 2 estimated parameters, just to illustrate how to use the package.
#' A more complex example with simultaneous estimation of specific and varietal plant parameters from a multi-varietal dataset is presented in another [vignette](https://SticsRPacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html).
#'
#' **Please note that parameter estimation on intercrops and chained situations (e.g. rotations) are not possible for the moment with the Stics model. It will be provided in next versions.**
#'
#' Data comes from a maize crop experiment (see description in Wallach et al., 2011).
#'
#' The parameter estimation is performed using the Nelder-Meade simplex method implemented in the nloptr package.
#'
