#' 
#' 
#' ## Run the parameter estimation
#' 
#' In this case, the parameter estimation algorithm (`optim_method`argument) and the criterion function (`crit_function` argument) must be set in input of `estim_param` function.
#' 
#' The list of available criteria for Bayesian methods is given by `? likelihoods`
#' 
#' The `param_info` argument has the same content as in the [specific and varietal parameters estimation vignette](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html).
#' 
#' **For the moment, only uniform distributions of bounds `param_info$lb` and `param_info$ub` can be used. Other type of distributions will be provided in next versions.** 
#' 
## ----results='hide', message=FALSE, warning=FALSE----------------------
optim_results=estim_param(obs_list=obs_list,
                          crit_function=likelihood_log_ciidn,
                          model_function=stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          optim_method="BayesianTools.dreamzs",
                          param_info=param_info)
