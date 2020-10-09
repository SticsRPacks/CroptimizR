## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  #' @title My model wrapper for CroptimizR
#  #'
#  #' @description This function runs my crop model on a set of situations (i.e. environments)
#  #' using the values of the parameters defined in the param_values argument. It returns
#  #' the values of the simulated outputs.
#  #'
#  #' @param param_values (optional) a named vector that contains the value(s) and name(s)
#  #' of the parameters to force for each situation to simulate. If not provided (or if is
#  #' NULL), the simulations will be performed using default values of the parameters
#  #' (e.g. as read in the model input files).
#  #'
#  #' @param sit_names Vector of situations names for which results must be returned.
#  #'
#  #' @param model_options List containing any information needed to run the model
#  #' (e.g. path to model input files and executable, ...)
#  #'
#  #' @return A list containing:
#  #'     o `sim_list`: a `named list` (names = situations names) of data.frames (or tibbles) of
#  #` simulated output values (one element of the list per simulated situation)
#  #'     o `error`: an error code indicating if at least one simulation ended with an error.
#  
#  model_wrapper <- function( param_values=NULL, sit_names, model_options, ...) {
#  
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  sim_list$situation1
#  # A tibble: *** x ***
#     Date         var1    var2    var3  ...
#     <dttm>     <dbl>   <dbl>   <dbl>
#   1 1994-10-17   0      2.53    4.80
#   2 1994-10-18   0      2.31    4.66
#   3 1994-10-19   0      4.55    4.44
#  
#  
#  sim_list$situation2
#  # A tibble: *** x ***
#     Date         var1    var2    var3  ...
#     <dttm>     <dbl>   <dbl>   <dbl>
#   1 1995-10-17   0      2.60    4.80
#   2 1995-10-18   0      3.42    4.70
#   3 1995-10-19   0      5.25    4.45
#  
#  
#  sim_list$situation3
#  # A tibble: *** x ***
#     Date         var1    var2    var3  ...
#     <dttm>     <dbl>   <dbl>   <dbl>
#   1 1996-10-17   0      2.41    4.81
#   2 1996-10-18   0      3.03    4.71
#   3 1996-10-19   0      5.10    4.47
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  model_wrapper <- function( param_values=NULL, sit_names, model_options, ...) {
#  
#    # Initializations
#    param_names <- names(param_values)
#    results <- list(sim_list = setNames(vector("list",length(sit_names)), nm = sit_names)),
#                   error=FALSE)
#  
#    for (situation in sit_names) {
#  
#        # overwrite model input parameters of names contained in param_names with values
#        # retrieved in param_values
#  
#        # run the model for the given situation
#  
#        # read the results and store the data.frame in result$sim_list[[situation]]
#  
#        if (any_error_returned_by_the model_or_detected_in_its_results) {
#          warning("any_useful_information_to_describe_the_error")
#          results$error=TRUE
#        }
#  
#      }
#  
#    }
#  
#    return(results)
#  
#  }
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  param_names<-    # set the name of one or several model input parameters in a vector
#  param_lb<-       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
#  param_ub<-       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
#  var_name<-       # give the name of an output variable sensitive to this (or these) parameter(s)
#  situation_name<- # give the name of the situation to simulate
#  model_options<-  # give the model options
#  wrapper<-        # give the name of your wrapper
#  
#  
#  param_values_min <- setNames(param_lb, param_names)
#  param_values_max <- setNames(param_ub, param_names)
#  sim_min       <- wrapper(param_values = param_values_min, model_options = model_options,
#                           sit_names=situation_name)
#  sim_max       <- wrapper(param_values = param_values_max, model_options = model_options,
#                           sit_names=situation_name)
#  
#  print(paste("Sum of differences, variable",var_name,", situation",situation_name," = ",
#               sum(abs(sim_max$sim_list[[situation_name]][,var_name] -
#                       sim_min$sim_list[[situation_name]][,var_name]),na.rm=TRUE)))
#  # Should give a value different from 0.
#  

## ----eval=FALSE---------------------------------------------------------------
#  # A tibble: 4 x 4
#    situation  p1     p2     p3
#    <chr>     <dbl>  <dbl>  <dbl>
#  1 sit1      50.14  1.14   340.43
#  2 sit2      55.37  1.23   126.47
#  3 sit3      43.22  2.12   234.56
#  4 sit4      38.49  2.02   236.45

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # convert param_values in a tibble
#  param_values <- tibble::tibble(!!!param_values)
#  
#  # loop on the situations to simulate
#  for (sit in situation_list) {
#  
#    # extract the parameters values to apply for the given situation
#    params <- NULL
#    if (!is.null(param_values)) {
#      if (! "situation" %in% names(param_values)) {
#        params <- param_values
#      } else {
#        params <- dplyr::filter(param_values, situation==sit) %>%
#          dplyr::select(-situation)
#      }
#    }
#  
#    # run the model with parameters values defined by params
#  
#  }

## ----warning=FALSE------------------------------------------------------------
# Code of an example of foreach loop algo
library("doParallel")

test_parallel <- function(cores_nb = 1,
                          pa = FALSE,
                          max_it = 5) {
# pa: flag to switch from case with pre-allocated list to case with returned statement
  
  
  # Launching the cluster
  cl <- makeCluster(cores_nb)
  registerDoParallel(cl)
  
  # List preallocation
  out_pa <- vector(mode = "list", max_it)
  
  # Parallel loop
  out <- foreach(i = 1:max_it) %dopar% {
    if (pa) {  
      out_pa[[i]] <- i  # store results in a pre-allocted list
    } else {
      return(i)         # return the results
    }
  }
  
  # Stopping the cluster
  stopCluster(cl)
  
  if (pa) {
    return(out_pa)
  } else {
    return(out)
  }
  
  
}

cores_nb=2

# Case with returned statement
out <- test_parallel(cores_nb)

# Case with pre-allocated list
out_pa <- test_parallel(cores_nb, TRUE)

out

out_pa


## ----eval=FALSE---------------------------------------------------------------
#  
#  lai_toymodel <- function(year, max_lai=8, julday_incslope=100, inc_slope=5,
#                           julday_decslope=200, dec_slope=2) {
#    # Simulate lai for a single crop over 2 years (from 01/01/year to 31/12/(year+1)
#    # with a simple double-logistic function
#    #
#    # Arguments
#    #   - year: first year of simulation
#    #   - max_lai: max value for lai
#    #   - inc_slope and dec_slope: increasing and decreasing slope
#    #   - julday_incslope and julday_decslope: julian days of maximal increasing and
#    #     decreasing slopes
#    #
#    # Value
#    #   - lai: vector of simulated lai
#    #   - dates: vector of dates (POSIXct) for which lai is computed
#  
#    end_day <- format(as.Date(paste0(year+1,"-12-31"), format = "%Y-%m-%d", origin=paste0(year,"-01-01")), "%j")
#    jul_days <- 1:as.numeric(end_day)
#  
#    lai <- max_lai * ( 1/(1+exp((julday_incslope-jul_days)/inc_slope)) -
#                       1/(1+exp((julday_decslope-jul_days)/dec_slope)) )
#    lai[lai<0] <- 0
#  
#    dates <- as.POSIXct(as.character(as.Date(jul_days,
#                                             origin=paste0(year,"-01-01"))),
#                        format = "%Y-%m-%d",tz="UTC")
#  
#    return(list(dates=dates, lai=lai))
#  
#  }
#  
#  
#  laitm_simple_wrapper <- function(param_values=NULL, sit_names, model_options, ...) {
#  
#    # A basic wrapper for lai_toymodel
#    #
#    # Arguments
#    #   - param_values: (optional) named vector containing the values of the lay_toymodel
#    #     parameters to force among max_lai, inc_slope, dec_slope, julday_incslope and
#    #     julday_decslope
#    #   - sit_names: Vector of situations names for which results must be returned.
#    #     In this case, the names of the situations are coded as "year_suffix"
#    #   - model_options: not used in this case
#    #   - ...: mandatory since CroptimizR will give additional arguments not used here
#    #
#    # Value:
#    #   A named list of tibble per situation.
#    #   Each tibble contains columns:
#    #      - Date (POSIXct dates of simulated results),
#    #      - One column per simulated variable (lai in this case)
#    #
#    # Details:
#    #  - Runs the lai_toymodel for a set of situations defined in sit_names
#    #  - Forces the parameters of lai_toymodel with the values given in param_values
#    #    argument
#    #  - Returns the required simulated values
#    #
#  
#    results <- list(sim_list = setNames(vector("list",length(sit_names)), nm = sit_names),
#                   error=FALSE)
#  
#    for (sit in sit_names) {
#  
#      # Retrieve year, emergence and crop_duration from situation name
#      tmp <- stringr::str_split(sit,"_")
#      year <- as.numeric(tmp[[1]][[1]])
#  
#      # Check inputs
#      if (year<1) {
#        warning(paste("sit_name",sit,
#                      "not well defined, first part is supposed to be a year!"))
#        results$error=TRUE
#        return(results)
#      }
#      if (!all(names(param_values) %in% c("max_lai", "inc_slope", "dec_slope",
#                                          "julday_incslope", "julday_decslope"))) {
#        warning(paste("Unknown parameters in param_values:",
#                      paste(names(param_values),collapse = ",")))
#        results$error=TRUE
#        return(results)
#      }
#  
#      # Call the lai_toymodel with varying arguments depending on what is given in
#      # param_values
#      res_laitm <- do.call('lai_toymodel', c(as.list(param_values),
#                                             list(year=year)))
#  
#      # Fill the result variable
#      results$sim_list[[sit]] <-  dplyr::tibble(Date=res_laitm$dates,
#                                                lai=res_laitm$lai)
#  
#    }
#  
#    return(results)
#  
#  }

## ----eval=FALSE---------------------------------------------------------------
#  tmp <- laitm_simple_wrapper(sit_names="2005_a", param_values = c(inc_slope=25, dec_slope=10))
#  
#  # Create synthetic observations by selecting simulated results
#  ind <- sort(sample(nrow(tmp$sim_list$`2005_a`),50))
#  obs_synth <- list(`2005_a`=tmp$sim_list$`2005_a`[ind,])
#  
#  # Try to retrieve inc_slope and dec_slope values
#  param_info <- list(lb=c(inc_slope=1,dec_slope=1), ub=c(inc_slope=100,dec_slope=100))
#  optim_options <- list(nb_rep=5, maxeval=100, xtol_rel=1e-2)
#  res <- estim_param(obs_synth, crit_function = crit_ols,
#              model_function = laitm_simple_wrapper,
#              optim_options=optim_options,
#              param_info = param_info)
#  res$final_values

## ----eval=FALSE---------------------------------------------------------------
#  laitm_simple_wrapper_v2 <- function(param_values=NULL, sit_names, model_options, ...) {
#  
#    # A basic wrapper for lai_toymodel
#    #
#    # Arguments
#    #   - param_values: (optional) a named vector or a tibble containing the values of the
#    #     lay_toymodel parameters to force among max_lai, inc_slope, dec_slope,
#    #     julday_incslope and julday_decslope. An optional column named Situation containing
#    #     the name of the situations allows to define different values of the parameters
#    #     for different situations.
#    #   - sit_names: Vector of situations names for which results must be returned.
#    #     In this case, the names of the situations are coded as "year_suffix"
#    #   - model_options: not used in this case
#    #   - ...: mandatory since CroptimizR will give additional arguments not used here
#    #
#    # Value:
#    #   A named list of tibble per situation.
#    #   Each tibble contains columns:
#    #      - Date (POSIXct dates of simulated results),
#    #      - One column per simulated variable (lai in this case)
#    #
#    # Details:
#    #  - Runs the lai_toymodel for a set of situations defined in sit_names
#    #  - Forces the parameters of lai_toymodel with the values given in param_values
#    #   argument
#    #  - Returns the required simulated values
#    #
#  
#    results <- list(sim_list = setNames(vector("list",length(sit_names)),
#                                        nm = sit_names), error=FALSE)
#  
#    param_values <- tibble::tibble(!!!param_values)  # convert param_values in a tibble
#  
#    for (sit in sit_names) {
#  
#      # Retrieve year, emergence and crop_duration from situation name
#      tmp <- stringr::str_split(sit,"_")
#      year <- as.numeric(tmp[[1]][[1]])
#  
#      # Check inputs
#      if (year<1) {
#        warning(paste("sit_name",sit,
#                      "not well defined, first part is supposed to be a year!"))
#        results$error=TRUE
#        return(results)
#      }
#  
#      # extract the parameters values to apply for the given situation
#      params <- NULL
#      if (!is.null(param_values)) {
#        if (! "situation" %in% names(param_values)) {
#          params <- param_values
#        } else {
#          params <- dplyr::filter(param_values, situation==sit) %>%
#            dplyr::select(-situation)
#        }
#      }
#      if (!all(names(params) %in% c("max_lai", "inc_slope", "dec_slope",
#                                    "julday_incslope", "julday_decslope"))) {
#        warning(paste("Unknown parameters in param_values:",
#                      paste(names(param_values),collapse = ",")))
#        results$error=TRUE
#        return(results)
#      }
#  
#      # Call the lai_toymodel with varying arguments depending on what is given in
#      # param_values
#      res_laitm <- do.call('lai_toymodel', c(as.list(params),
#                                             list(year=year)))
#  
#      # Fill the result variable
#      results$sim_list[[sit]] <-  dplyr::tibble(Date=res_laitm$dates,
#                                                lai=res_laitm$lai)
#  
#    }
#  
#    return(results)
#  
#  }

## ----eval=FALSE---------------------------------------------------------------
#  tmp <- laitm_simple_wrapper_v2(sit_names=c("2005_a","2006_b"),
#                              param_values = dplyr::tibble(situation=c("2005_a","2006_b"),
#                                                    inc_slope=c(25,50), dec_slope=c(10,10)))
#  
#  # Create synthetic observations by selecting simulated results
#  length_2005_a <- nrow(tmp$sim_list$`2005_a`)
#  length_2006_b <- nrow(tmp$sim_list$`2006_b`)
#  obs_synth <- list(`2005_a`=tmp$sim_list$`2005_a`[seq(from=1, to=length_2005_a, by=3),],
#                    `2006_b`=tmp$sim_list$`2006_b`[seq(from=1, to=length_2006_b, by=3),])
#  
#  # Try to retrieve inc_slope and dec_slope values on both situations
#  param_info=list(inc_slope=list(sit_list=list("2005_a","2006_b"),
#                            lb=c(1,1),ub=c(100,100)),
#                  dec_slope=list(sit_list=list(c("2005_a","2006_b")),
#                            lb=1,ub=100))
#  
#  optim_options <- list(nb_rep=5, maxeval=100, xtol_rel=1e-2)
#  res <- estim_param(obs_synth, crit_function = crit_ols,
#              model_function = laitm_simple_wrapper_v2,
#              optim_options=optim_options,
#              param_info = param_info)
#  res$final_values

