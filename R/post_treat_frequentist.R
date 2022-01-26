#' @title Post-treat results of frequentist methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by frequentist method wrappers
#' @param crit_options List containing several arguments given to `estim_param` function:
#' `param_names`, `obs_list`, `crit_function`, `model_function`, `model_options`,
#' `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return Updated results of frequentist method
#'
post_treat_frequentist <- function(optim_options, param_info, optim_results,
                                   crit_options) {

  param_names <- get_params_names(param_info)
  nb_params=length(param_names)
  crit_function <- crit_options$crit_function
  info_crit_list <- crit_options$info_crit_list

  # Recompute final value of minimized criterion
  # (just to check it is correct and to get the observation list used)
  info_final <- main_crit(param_values=optim_results$final_values,
                          crit_options=c(crit_options,return_obs_sim=TRUE))
  if (info_final$crit != optim_results$min_crit_value) {
    stop(paste("Internal error: incoherent computation of minimum criterion value. \nValue obtained in method wrapper:",
               optim_results$min_crit_value, "\nValue obtained afterwards:", info_final$crit))
  }

  if (identical(crit_function, crit_ols)) {
    sapply(info_crit_list, function(x) {
      final_info_crit <- x(obs_list=info_final$obs_intersect,
                            crit=info_final$crit,
                            param_nb=nb_params)
      optim_results[x()$name] <<- final_info_crit
    })
  }

  return(optim_results)

}
