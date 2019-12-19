#' @title Call the required parameter estimation method
#'
#' @inheritParams estim_param
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#'
#' @return prints, graphs and a list containing the results of the parameter estimation.
#' which content depends on the method used.
#'   e.g. for Nelder meade simplex in nloptr, this list contains
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `est_values`, the vector of estimated values for optimized parameters
#' for all repetitions
#' `ind_min_crit`, the index of the repetition that lead to the lowest value
#' of the criterion
#' `nlo`, the data structure returned by nloptr
#'
#' @keywords internal
#'

optim_switch <- function(param_names,obs_list,crit_function,model_function,model_options=NULL,optim_method="nloptr.simplex",optim_options=NULL,prior_information) {

  if (optim_method=="nloptr.simplex" || optim_method=="simplex") {

    res=wrap_nloptr(param_names=param_names,obs_list=obs_list,crit_function=crit_function,model_function=model_function,model_options=model_options,
                optim_options=optim_options,prior_information=prior_information)

  } else if (optim_method=="BayesianTools.dreamzs" || optim_method=="dreamzs") {

    res=wrap_BayesianTools(param_names=param_names,obs_list=obs_list,crit_function=crit_function,model_function=model_function,model_options=model_options,
                optim_options=optim_options,prior_information=prior_information)

  } else {

    stop(paste0("Unknown method ",optim_method,", please choose between nloptr.simplex and BayesianTools.dreamzs."))

  }

  return(res)

}
