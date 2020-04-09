

#' 
#' ## Run the optimization
#' 
#' The Nelder-Meade simplex is the default method => no need to set the 
#' optim_method argument. For the moment it is the only method interfaced (others will come soonly).
#' Same for crit_function: a value is set by default (`crit_log_cwss`, see `? crit_log_cwss` for more details and list of available criteria). Others will be proposed in next versions of CroptimizR. The user can implement and give in argument its own criterion (see inputs and outputs required in the `crit_log_cwss` function).
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------

optim_results=estim_param(obs_list=obs_list,
                            model_function=apsimx_wrapper,
                            model_options=model_options,
                            optim_options=optim_options,
                            param_info=param_info)

