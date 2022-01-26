#' @title Summarizes results of frequentist methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by frequentist method wrappers
#'
#' @return Prints results of frequentist methods
#'
summary_frequentist <- function(optim_options, param_info, optim_results) {

  param_names <- get_params_names(param_info)
  nb_params=length(param_names)
  bounds=get_params_bounds(param_info)
  path_results <- optim_options$path_results
  init_values <- optim_results$init_values
  est_values <- optim_results$est_values
  crit_values <- optim_results$crit_values
  ind_min_crit <- optim_results$ind_min_crit
  min_crit_value <- optim_results$min_crit_value

  # Display of parameters values for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit,ipar]))
  }
  print(paste("Minimum value of the criterion:", min_crit_value))
  print(paste("Complementary graphs and results can be found in ", path_results))

}
