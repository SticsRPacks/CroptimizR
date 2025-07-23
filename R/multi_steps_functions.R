#' @title Summarizes results of multi-step procedure
#'
#' @inheritParams estim_param
#'
#' @param results_multi_step Results of the multi_step procedure as returned by post_treat_multi_step
#'
#' @param path_results Folder path where results of the multi-step optimization process can be found
#'
#' @return Prints results of the multi-step procedure
#'
summary_multi_step <- function(results_multi_step, path_results) {
  cat(paste(
    "\nList of observed variables used:",
    paste(results_multi_step$obs_var_list, collapse = ", ")
  ))

  # Display of estimated parameters values
  param_names <- names(results_multi_step$final_values)
  for (par in param_names) {
    cat(paste(
      "\nEstimated value for", par, ": ",
      format(results_multi_step$final_values[[par]],
        scientific = FALSE, digits = 2, nsmall = 0
      )
    ))
  }

  # Display of forced parameters values
  param_names <- names(results_multi_step$forced_param_values)
  for (par in param_names) {
    cat(paste(
      "\nForced value for", par, ": ",
      format(results_multi_step$forced_param_values[[par]],
        scientific = FALSE, digits = 2, nsmall = 0
      )
    ))
  }

  cat(paste(
    "\nComplementary graphs and results can be found in ",
    path_results, "\n"
  ))
}


#' @title Post-treat results of multi-step procedure
#'
#' @inheritParams estim_param
#'
#' @param step List of steps of the multi-step procedure
#' @param optim_results_list List of results returned for each step of the multi-step parameter estimation process
#'
#' @return List of estimated and forced parameters values
#'
post_treat_multi_step <- function(step, optim_results_list) {
  res <- list()

  # Concatenate the list of estimated values
  res$final_values <- unlist(lapply(optim_results_list, function(x) x$final_values))

  # Concatenate the list of forced values, remove the estimated parameters from the list, and compute them
  forced_param_values <- unlist(lapply(optim_results_list, function(x) x$forced_param_values))
  forced_param_values <- forced_param_values[setdiff(names(forced_param_values), names(res$final_values))]
  res$forced_param_values <- compute_eq_const(forced_param_values, res$final_values)

  # Concatenate the list of observations used
  res$obs_var_list <- unique(unlist(lapply(optim_results_list, function(x) x$obs_var_list)))

  # Store the definition and results of all steps
  res$step <- step
  lapply(seq(optim_results_list), function(x) {
    res$step[[x]]$optim_results <<- optim_results_list[[x]]
  })

  return(res)
}
