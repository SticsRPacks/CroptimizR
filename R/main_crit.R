main_crit <- function(param_values,crit_options) {
  #' @title main function for criterion to optimize
  #'
  #' @param param_values Value(s) of the parameters
  #' @param crit_options A list containing the following elements:
  #' param_names Name(s) of parameters
  #' obs_list List of observed values
  #' crit_function Function implementing the criterion to optimize
  #' model_function Function implementing the criterion to optimize
  #' model_options List of arguments to pass to model function
  #' situation_names Name(s) of the situations to simulate
  #'
  #' @return The value of the criterion
  #'
  #' @export
  #'
  #' @examples

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

  names(param_values)=param_names

  # Call model function
  model_results=model_function(param_values=param_values,sit_var_dates=obs_list,
                 model_options=model_options)

  # intersect sim and obs if necessary
  if (!model_results$flag_allsim) {
    obs_sim_list=intersect_sim_obs(model_results$sim_list,obs_list)
  }

  # Compute criterion value
  return(crit_function(obs_sim_list$sim_list,obs_list))

}
