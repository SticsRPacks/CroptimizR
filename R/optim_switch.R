#' @title Call the required parameter estimation method
#'
#' @inheritParams estim_param
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#' @param crit_options List containing several arguments given to `estim_param` function:
#' `param_names`, `obs_list`, `crit_function`, `model_function`, `model_options`,
#' `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return prints, graphs and a list containing the results of the parameter estimation,
#' which content depends on the method used, all that saved in the defined in
#' `optim_options.path_results`
#'
#' @keywords internal
#'

optim_switch <- function(param_names,optim_method,optim_options,param_info,crit_options) {

  if (optim_method=="nloptr.simplex" || optim_method=="simplex") {

    res=wrap_nloptr(param_names=param_names,optim_options=optim_options,param_info=param_info, crit_options)

  } else if (optim_method=="BayesianTools.dreamzs" || optim_method=="dreamzs") {

    res=wrap_BayesianTools(param_names=param_names,optim_options=optim_options,param_info=param_info, crit_options)

  } else if (optim_method=="optim") {

    res=wrap_optim(param_names=param_names,optim_options=optim_options,param_info=param_info, crit_options)

  } else if (optim_method=="genoud") {

    res=wrap_rgenoud(param_names=param_names,optim_options=optim_options,param_info=param_info, crit_options)

  } else {

    stop(paste0("Unknown method ",optim_method,", please choose between nloptr.simplex, BayesianTools.dreamzs and optim."))

  }

  return(res)

}
