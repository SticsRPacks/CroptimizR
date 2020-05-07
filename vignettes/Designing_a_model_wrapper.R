## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  #' @title MyModel wrapper for CroptimizR
#  #'
#  #' @description This function runs my crop model and force it with the values
#  #' of the parameters defined in the param_values argument. It returns
#  #' the values of the simulated outputs.
#  #'
#  #' @param model_options List containing any information needed to run the model.
#  #'
#  #' @param param_values Named 3D array that contains the value(s) and names of the
#  #' parameters to force for each situation to simulate. This array contains the different
#  #' parameters values (first dimension) for the different parameters (second dimension)
#  #' and for the different situations (third dimension).
#  #'
#  #' @return A list containing simulated values (`sim_list`: a vector of list (one
#  #' element per values of parameters) containing data.frames of simulated output values
#  #' for each simulated situation) and an error code (`error`) indicating if at least
#  #' one simulation ended with an error.
#  
#  model_wrapper <- function( model_options, param_values, ...) {
#  
#  }
#  

## -----------------------------------------------------------------------------
param_values <- array( c(0.001, 0.002, 50, 50,
                         0.001, 0.002, 60, 60,
                         0.001, 0.002, 70, 70),
                       dim=c(2,2,3),
                       dimnames=list(NULL,c("param1", "param2"),c("situation1", "situation2", "situation3")))

param_values

## ----eval=FALSE---------------------------------------------------------------
#  
#  $sim_list
#  $sim_list[[1]]
#  $sim_list[[1]]$situation1
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1994-10-17 00:00:00  0      2.53    4.80
#  #> 2 1994-10-18 00:00:00  0      2.31    4.66
#  #> 3 1994-10-19 00:00:00  0      4.55    4.44
#  #
#  
#  $sim_list[[1]]$situation2
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1995-10-17 00:00:00  0      2.60    4.80
#  #> 2 1995-10-18 00:00:00  0      3.42    4.70
#  #> 3 1995-10-19 00:00:00  0      5.25    4.45
#  #
#  
#  $sim_list[[1]]$situation3
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1996-10-17 00:00:00  0      2.41    4.81
#  #> 2 1996-10-18 00:00:00  0      3.03    4.71
#  #> 3 1996-10-19 00:00:00  0      5.10    4.47
#  #
#  
#  
#  $sim_list[[2]]
#  $sim_list[[2]]$situation1
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1994-10-17 00:00:00  0.1    2.55    4.80
#  #> 2 1994-10-18 00:00:00  0.1    2.32    4.66
#  #> 3 1994-10-19 00:00:00  0.1    4.57    4.44
#  #
#  
#  $sim_list[[2]]$situation2
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1995-10-17 00:00:00  0.1    2.62    4.80
#  #> 2 1995-10-18 00:00:00  0.1    3.40    4.70
#  #> 3 1995-10-19 00:00:00  0.1    5.26    4.45
#  #
#  
#  $sim_list[[2]]$situation3
#  # A tibble: *** x ***
#     Date                  var1    var2    var3  ...
#  #>   <dttm>              <dbl>   <dbl>   <dbl>
#  #> 1 1996-10-17 00:00:00  0.1    2.42    4.81
#  #> 2 1996-10-18 00:00:00  0.1    3.04    4.71
#  #> 3 1996-10-19 00:00:00  0.1    5.11    4.47
#  #
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  model_wrapper <- function( model_options, param_values, ...) {
#  
#    # Initializations
#    nb_paramValues <- dim(param_values)[1]
#    param_names <- dimnames(param_values)[[2]]
#    situation_names <- dimnames(param_values)[[3]]
#    result$sim_list <- vector("list",dim(param_values)[1])
#    results$error=FALSE
#  
#    for (i in 1:nb_paramValues) {
#  
#      for (situation in situation_names) {
#  
#        # overwrite model input parameters of names contained in param_names with values retrieved in param_values[i,,situation]
#  
#        # run the model for the given situation
#  
#        # read the results and store the data.frame in result$sim_list[[i]][[situation]]
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
#  param_names=    # set the name of one or several model input parameters in a vector
#  param_lb=       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
#  param_ub=       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
#  var_name=       # give the name of an output variable sensitive to this (or these) parameter(s)
#  situation_name= # give the name of the situation to simulate
#  model_options=  # give the model options
#  wrapper=        # give the name of tyour wrapper
#  
#  
#  param_values_min <- array( param_lb,
#                         dim=c(1,length(param_lb),1),
#                         dimnames=list(NULL,param_names,situation_name))
#  
#  param_values_max <- array( param_ub,
#                         dim=c(1,length(param_ub),1),
#                         dimnames=list(NULL,param_names,situation_name))
#  
#  sim_max       <- wrapper(param_values = param_values_min, model_options = model_options)
#  
#  sim_min       <- wrapper(param_values = param_values_max, model_options = model_options)
#  
#  print(paste("Sum of differences, variable",var_name,", situation",situation_name," = ",
#               sum(abs(sim_max$sim_list[[1]][[situation_name]][,var_name]-sim_min$sim_list[[1]][[situation_name]][,var_name]),na.rm=TRUE)))
#  # Should give a value different from 0.
#  

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


