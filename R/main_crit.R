#' @title main function for criterion to optimize
#'
#' @param param_values Value(s) of the parameters
#' @param crit_options A list containing the following elements:
#' `param_names` Name(s) of parameters
#' `obs_list` List of observed values
#' `crit_function` Function implementing the criterion to optimize
#' `model_function` Function implementing the criterion to optimize
#' `model_options` List of arguments to pass to model function
#' `situation_names` Name(s) of the situations to simulate
#' `param_info` Information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing a vector of upper and lower
#' bounds (`ub` and `lb`), or a list of list containing for each
#' parameter and group of situation the names of the situations (`sit_names`)
#' and upper and lower bounds (`ub` and `lb`)
#'
#' @return The value of the criterion
#'
#' @keywords internal
#'
main_crit <- function(param_values, crit_options) {

  # Initializations
  param_names <- crit_options$param_names
  obs_list <- crit_options$obs_list
  crit_function <- crit_options$crit_function
  model_function <- crit_options$model_function
  model_options <- crit_options$model_options
  param_info <- crit_options$param_info
  transform_obs <- crit_options$transform_obs
  transform_sim <- crit_options$transform_sim

  names(param_values) <- param_names
  situation_names <- names(obs_list)
  nb_situations=length(situation_names)
  param_names_sl=get_params_names(param_info, short_list = TRUE)
  nb_params_sl=length(param_names_sl)

  # Denormalize parameters
  # TO DO

  # Apply constraints on the parameters
  # TO DO

  # Transform parameters
  # TO DO

  # Distribute the param values in a big array storing param values per usms
  param_values_df=sapply(situation_names, function(x) CroptimizR:::get_params_per_sit(param_info,x,param_values))
  param_values_array=array(unlist(param_values_df),dim=c(1,nb_params_sl,nb_situations),
                           dimnames=list(NULL,param_names_sl,situation_names))

  # Call model function
  model_results <- model_function(model_options = model_options,
                                  param_values = param_values_array,
                                  sit_var_dates_mask = obs_list)

  # Transform simulations
  if (!is.null(transform_sim)) {
    model_results <- transform_sim(model_results=model_results, obs_list=obs_list, param_values=param_values, model_options=model_options)
  }

  # Check results
  if (!is.null(model_results$error) && model_results$error) {
    warning("Error in model simulations")
  }
  if (is.null(model_results$sim_list[[1]]) || length(model_results$sim_list[[1]])==0) {
    stop("Error: model wrapper returned an empty list!")
  }
  if (!is.sim(model_results$sim_list)) {
    stop("Error: format of results returned by the model wrapper is incorrect!")
  }

  # Transform observations
  if (!is.null(transform_obs)) {
    obs_list <- transform_obs(model_results=model_results, obs_list=obs_list, param_values=param_values, model_options=model_options)
  }


  # intersect sim and obs
  obs_sim_list <- intersect_sim_obs(model_results$sim_list[[1]], obs_list)
  if (!is.list(obs_sim_list)) {
    stop("Error: intersection of simulations and observations is empty (no date and/or variable in common)!")
  }
  if (any(sapply(obs_sim_list$sim_list,function(x) any(sapply(x,is.nan)))) || any(sapply(obs_sim_list$sim_list,function(x) any(sapply(x,is.infinite))))) {
    stop("Error: the model wrapper returned NaN or infinite values: \n ",obs_sim_list$sim_list,"\n Estimated parameters: ",paste(param_names,collapse=" "),", values: ",paste(param_values, collapse=" "))
  }

  # Compute criterion value
  crit=crit_function(obs_sim_list$sim_list, obs_sim_list$obs_list)
  if (is.nan(crit)) {
    stop(paste0("Error: optimized criterion returned NaN value. \n Estimated parameters: ",paste(param_names,collapse=" "),", values: ",paste(param_values, collapse=" ")))
  }

  return(crit)
}
