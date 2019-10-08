main_optim <- function(param_names,obs_list,crit_function,model_function,model_options,optim_options,prior_information) {
  #' @title main function for criterion to optimize
  #'
  #' @param param_values Value(s) of the parameters to force (optional)
  #' @param param_names Name(s) of parameters to force the value (optional)
  #' @param obs_list List of observed values
  #' @param crit_function Function implementing the criterion to optimize
  #'
  #' @return The value of the criterion
  #'
  #' @export
  #'
  #' @examples

  #
  # TO ADD in arg :
  #    - optim method
  #    - prior information : for the moment ub, lb
  #

  # Loop on the stages
  ## force estimated parameters for next steps

  optim_switch(param_names,obs_list,crit_function,model_function,model_options,optim_options,prior_information)


}
