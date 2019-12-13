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
#' `prior_information` Prior information on the parameters to estimate.
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
main_crit <- function(param_values,crit_options) {

  # Denormalize parameters
  # TO DO

  # Apply constraints on the paremeters
  # TO DO

  # Transform parameters
  # TO DO


  param_names=crit_options$param_names
  obs_list=crit_options$obs_list
  crit_function=crit_options$crit_function
  model_function=crit_options$model_function
  model_options=crit_options$model_options
  prior_information=crit_options$prior_information

  names(param_values)=param_names

  # Call model function
  model_results=model_function(param_values=param_values,sit_var_dates_mask=obs_list,
                 prior_information=prior_information,model_options=model_options)

  # intersect sim and obs if necessary
  obs_sim_list=list(sim_list=model_results$sim_list,obs_list=obs_list)
  #if (!model_results$flag_allsim) {

    obs_sim_list=intersect_sim_obs(model_results$sim_list,obs_list)
    if (!is.list(obs_sim_list)) {
      stop("Error: intersection of simulations and observations is empty (no date and/or variable in common)!")
    }
  #}

  # Compute criterion value
  return(crit_function(obs_sim_list$sim_list,obs_sim_list$obs_list))

}
