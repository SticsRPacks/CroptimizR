#' @title Provide a list of candidate parameters to estimate based on the
#' Forward Selection algorithm proposed in AgMIP calibration phaseIII protocol
#' and a given information criterion
#'
#' @param oblig_param_list Vector of names of parameters that must be estimated
#' (list of obligatory parameters)
#' @param add_param_list Vector of names of additional parameters candidate to
#' the estimation
#' @param crt_list Vector of names of candidate parameters that have been
#' previously used in parameter estimation
#' @param info_crit_values Values of the information criterion obtained so far
#'  for the different steps.
#'
#' @details This function implements the Forward Selection based on information
#' criterion as described in AgMIP Calibration protocol phase III
#' (Crop model calibration using phenology data)
#'   If current information criterion value is superior to the previous one,
#'   we replace the previously added candidate parameter in crt_list by the next
#'   one in add_param_list.
#'   If current information criterion value is inferior to the previous one,
#'   we add a new candidate parameter to crt_list
#'
#'   A first estimation using oblig_param_list as candidate parameters must be
#'   performed before calling this function
#'
#' @return A list that contains:
#' (i) the names of the candidate parameters for next parameter estimation
#' or NULL when all parameters have been tested (next_candidates)
#' (ii) a flag that indicates if the previous parameters tested are selected
#' (selected)
#'
#' @importFrom utils tail head
#'
#' @keywords internal
#'
select_param_FwdRegAgMIP <- function(oblig_param_list, add_param_list, crt_list,
                                     info_crit_values) {
  res <- list()
  res$next_candidates <- NULL
  if (length(info_crit_values) > 1) {
    crt_info_crit <- tail(info_crit_values, 1)
    prev_info_crit <- head(info_crit_values, length(info_crit_values) - 1)
  }

  if (is.null(add_param_list)) {
    res$selected <- TRUE
    return(res)
  } else if (crt_list[length(crt_list)] ==
    add_param_list[length(add_param_list)]) {

    # we tested all parameters
    if (crt_info_crit < min(prev_info_crit)) {
      res$selected <- TRUE
    } else {
      res$selected <- FALSE
    }
    return(res)
  } else if (length(crt_list) == length(oblig_param_list)) {

    # we only tested so far the obligatory parameters
    res$selected <- TRUE
    res$next_candidates <- c(oblig_param_list, add_param_list[1])
  } else {
    if (crt_info_crit < min(prev_info_crit)) {
      # Add the next candidate to the list
      res$selected <- TRUE
      res$next_candidates <- c(
        crt_list,
        add_param_list[which(add_param_list == crt_list[length(crt_list)]) + 1]
      )
    } else {

      # Replace the last candidate parameter by the next candidate
      res$selected <- FALSE
      res$next_candidates <- c(
        crt_list[-length(crt_list)],
        add_param_list[which(add_param_list == crt_list[length(crt_list)]) + 1]
      )
    }
  }

  return(res)
}




#' @title Post-treat the results of the Forward Selection algorithm proposed in
#' AgMIP calibration phaseIII protocol
#'
#' @param optim_results Results list returned by frequentist method wrappers
#' @param crit_options List containing several arguments given to `estim_param`
#' function: `param_names`, `obs_list`, `crit_function`, `model_function`,
#' `model_options`, `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#' @param crt_list Vector of names of candidate parameters that have been
#' used in parameter estimation
#' @param param_selection_steps A tibble summarizing the results of the previous
#' previouslyparameter estimation steps as returned by the previous call to
#' this function, NULL if it is the first step.
#'
#' @return A tibble summarizing the results of the parameter estimation steps
#'
#' @importFrom stats setNames
#'
#' @keywords internal
#'
post_treat_FwdRegAgMIP <- function(optim_results, crit_options, crt_list,
                                   param_selection_steps) {
  info_crit_func <- crit_options$info_crit_list[[1]]
  final_info_crit <- optim_results[[info_crit_func()$name]]

  ## RE-compute main_crit with the initial values of the parameters
  init_crit <- main_crit(
    param_values = optim_results$init_values[optim_results$ind_min_crit, ],
    crit_options = c(crit_options, return_obs_sim = FALSE)
  )

  ## Store the results per step
  digits <- 2
  v_init <- as.vector(t(optim_results$init_values[optim_results$ind_min_crit, ]))
  names(v_init) <- names(optim_results$init_values)
  info_new_step <- setNames(
    tibble(
      list(crt_list),
      list(v_init),
      list(optim_results$final_values),
      init_crit,
      optim_results$min_crit_value,
      final_info_crit,
      ""
    ),
    c(
      "Estimated parameters", "Initial parameter values",
      "Final values",
      "Initial Sum of squared errors",
      "Final Sum of squared errors",
      info_crit_func()$name, "Selected step"
    )
  )
  param_selection_steps <- dplyr::bind_rows(param_selection_steps, info_new_step)
  ind_min_infocrit <- which.min(param_selection_steps[[info_crit_func()$name]])
  param_selection_steps[, "Selected step"] <- ""
  param_selection_steps[ind_min_infocrit, "Selected step"] <- "X"

  return(param_selection_steps)
}


#' @title Summarize the results of the Forward Selection algorithm proposed in
#' AgMIP calibration phaseIII protocol
#'
#' @inheritParams optim_switch
#'
#' @param param_selection_steps A tibble summarizing the results of the previous
#' parameter estimation steps as returned by the previous call to this function,
#' NULL if it is the first step.
#'
#' @return Print the results in standard output.
#'
#' @keywords internal
#'
summary_FwdRegAgMIP <- function(param_selection_steps,
                                info_crit_list, path_results) {
  cat("----------------------\n")
  cat("End of parameter selection process\n")
  cat("----------------------\n\n")

  ind_min_infocrit <-
    which.min(param_selection_steps[[info_crit_list[[1]]()$name]])
  cat("Selected step:", ind_min_infocrit, "\n")
  selected_param <-
    param_selection_steps$`Estimated parameters`[[ind_min_infocrit]]
  cat("Selected parameters:", paste(selected_param, collapse = ","), "\n")
  param_values <- param_selection_steps$`Final values`[[ind_min_infocrit]]
  nb_params <- length(param_values)
  for (ipar in 1:nb_params) {
    cat(
      "Estimated value for", selected_param[ipar],
      ": ", format(param_values[ipar],
        scientific = FALSE,
        digits = 2, nsmall = 2
      ), "\n"
    )
  }
}




#' @title Save the results of the Forward Selection algorithm proposed in
#' AgMIP calibration phaseIII protocol
#'
#' @param param_selection_steps A tibble summarizing the results of the previous
#' parameter estimation steps as returned by the previous call to this function,
#' NULL if it is the first step.
#' @param path_results Path of the folder where to store the results
#'
#' @return Save param_selection_steps in a csv file in folder path_results
#'
#' @importFrom utils write.table
#' @importFrom purrr modify
#'
#' @keywords internal
#'
save_results_FwdRegAgMIP <- function(param_selection_steps, path_results) {
  tb <- purrr::modify_if(
    param_selection_steps,
    function(x) !is.list(x), as.list
  )
  # format everything in char and 2 digits
  tb <- purrr::modify(
    tb,
    function(x) {
      unlist(
        purrr::modify(x, function(y) {
          paste(format(y,
            scientific = FALSE,
            digits = 2, nsmall = 2
          ), collapse = ", ")
        })
      )
    }
  )

  utils::write.table(tb,
    sep = ";", file = file.path(
      path_results,
      "param_selection_steps.csv"
    ),
    row.names = FALSE
  )

  cat(
    "\nA table summarizing the results obtained at the different steps ",
    "is stored in ", file.path(path_results, "param_selection_steps.csv"), "\n"
  )
  cat(
    "Graphs and detailed results obtained for the different steps can be ",
    "found in ", file.path(path_results, "results_all_steps", "step_#"),
    "folders.\n\n"
  )
}
