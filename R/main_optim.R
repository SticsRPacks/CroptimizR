  #' @title main function for parameter estimation
  #'
  #' @param param_names Name(s) of parameters to estimate
  #' @param obs_list List of observed values
  #' @param crit_function Function implementing the criterion to optimize
  #' @param model_function Crop Model wrapper function
  #' @param model_options List of options for the Crop Model wrapper (optional,
  #' see help of the Crop Model wrapper used)
  #' @param optim_method Name of the parameter estimation method (optional,
  #' see help of optim_switch function)
  #' @param optim_options List of options of the parameter estimation method:
  #' \code{nb_rep}, the number of repetitions (optional, default=1)
  #' (see help of optim_switch function))
  #' @param prior_information Prior information on the parameters to estimate.
  #' For the moment only uniform distribution are allowed.
  #' Either a list containing (named) vectors of upper and lower
  #' bounds (\code{ub} and \code{lb}), or a named list containing for each
  #' parameter the list of situations per group (\code{sit_list})
  #' and the vector of upper and lower bounds (one value per group) (\code{ub} and \code{lb})
  #'
  #' @return Prints and graphs, depend on the parameter estimation method used
  #'
  #' @export
  #'
  #' @examples
  #'
  #' library(SticsRFiles)
  #' library(SticsOnR)
  #' library(SticsOptimizR)
  #' library(dplyr)
  #' library(nloptr)
  #'
  #' prior_information=list(lb=c(dlaimax=0.0005, durvieF=50),
  #'                        ub=c(dlaimax=0.0025, durvieF=400))
  #'
  #'
  #' model_options=list()
  #' model_options$stics_path="D:/Home/sbuis/Documents/WORK/STICS/JavaSTICS-1.41-stics-9.0/bin/stics_modulo"  # TO ADAPT TO YOUR CASE :-) ###
  #' model_options$data_dir=system.file(file.path("extdata","TestCase1c"), package = "SticsOptimizR")
  #'
  #' optim_options=list()
  #' optim_options$nb_rep <- 2 # How many times we run the minimization with different parameters
  #' optim_options$xtol_rel <- 1e-05 # Tolerance criterion between two iterations
  #' optim_options$maxeval <- 3 # Maximum number of iterations executed by the function
  #' optim_options$path_results <- model_options$data_dir # path where to store results graphs
  #'
  #' obs_list=read_obs_to_list(file.path(model_options$data_dir,"Orig Data"))
  #'
  #' main_optim(obs_list=obs_list,crit_function=concentrated_wss,model_function=stics_wrapper,model_options=model_options,optim_options=optim_options,prior_information=prior_information)


main_optim <- function(obs_list,crit_function,model_function,model_options=NULL,optim_method="simplex",optim_options=NULL,prior_information) {


  #
  # TO DO LIST
  #    - handle the case of intercrop
  #    - handle the case of rotations
  #    - handle the case of different steps (each argument maybe lists?)

  # Loop on the steps
  ## force estimated parameters for next steps

  param_names=get_params_names(prior_information)

  optim_switch(param_names,obs_list,crit_function,model_function,model_options,optim_method,optim_options,prior_information)

  }
