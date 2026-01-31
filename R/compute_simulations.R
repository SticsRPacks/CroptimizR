#' @title Compute simulated variables by running the model wrapper and applying
#' the required transformations.
#'
#' @inheritParams estim_param
#' @param param_values Named vector or tibble, value(s) of the parameters to
#' pass to the model wrapper
#' @param situation (optional) List of situations to take into account within obs_list.
#' situation = NULL means that all situations in obs_list will be used.
#' @param sit_var_dates_mask List of situations: a named list
#' containing a mask for variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared.
#'
#' @return A list containing the model outputs and transformed results, in the cropr_simulation format.
#' Returns NA if an error occurs during the simulations.
#'
#' @keywords internal
#'
compute_simulations <- function(model_function, model_options, param_values,
                                situation, var_to_simulate, obs_list,
                                transform_sim = NULL, transform_var = NULL,
                                sit_var_dates_mask = NULL) {
  if (is.null(var_to_simulate)) var_to_simulate <- setdiff(unique(unlist(lapply(obs_list, names))), "Date")

  arglist <- list(
    model_options = model_options, param_values = param_values,
    situation = situation, sit_names = situation,
    var = var_to_simulate, var_names = var_to_simulate,
    sit_var_dates_mask = sit_var_dates_mask
  )

  # Could be done only at the beginning in test_wrapper
  if (("sit_names" %in% names(formals(model_function))) |
    ("var_names" %in% names(formals(model_function)))) {
    lifecycle::deprecate_warn(
      "0.5.0", "model_function(sit_names)",
      "model_function(situation)"
    )
    lifecycle::deprecate_warn(
      "0.5.0", "model_function(var_names)",
      "model_function(var)"
    )
  }
  model_results <- do.call(model_function, args = arglist[intersect(
    names(formals(model_function)),
    names(arglist)
  )])
  # Check results, return NA if incorrect
  if (is.null(model_results) ||
    (!is.null(model_results$error) && model_results$error)) {
    warning(paste(
      "Error in model simulations for parameters values",
      paste0(param_values, collapse = ",")
    ))
    return(NA)
  }
  if (is.null(model_results$sim_list) || length(model_results$sim_list) == 0) {
    warning(paste(
      "Model wrapper returned an empty list for parameters values",
      paste0(param_values, collapse = ",")
    ))
    return(NA)
  }
  if (!is.sim(model_results$sim_list)) {
    warning("Format of results returned by the model wrapper is incorrect!")
    return(NA)
  }

  # Transform simulations
  sim_transformed <- NULL
  if (!is.null(transform_sim)) {
    sim_transformed <- tryCatch(
      transform_sim(
        model_results = model_results, obs_list = obs_list,
        param_values = param_values,
        model_options = model_options
      ),
      error = function(cond) {
        message(paste("Caught an error while executing the user function for transforming
        simulations (argument transform_sim of estim_param function). \n
                 param_values=", paste(param_values, collapse = ",")))
        print(cond)
        stop()
      }
    )
  }

  # Transform variables if necessary
  if (!is.null(transform_var)) {
    if (is.null(sim_transformed)) {
      to_transform <- model_results
    } else {
      to_transform <- sim_transformed
    }
    sim_transformed$sim_list <- lapply(to_transform$sim_list, function(x) {
      for (var in intersect(names(x), names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
    sim_transformed$error <- to_transform$error
    attr(sim_transformed$sim_list, "class") <- "cropr_simulation"
  }

  # Check results, return NA if incorrect
  if (!is.null(transform_sim) || !is.null(transform_var)) {
    if (is.null(sim_transformed) ||
      (!is.null(sim_transformed$error) && sim_transformed$error)) {
      warning("Error in transformation of simulation results.")
      return(NA)
    }
    if (is.null(sim_transformed$sim_list) || length(sim_transformed$sim_list) == 0) {
      warning("Transformation of simulation results returned an empty list!")
      return(NA)
    }
    if (!is.sim(sim_transformed$sim_list)) {
      warning("Format of results returned by transformation of model results is incorrect!")
      return(NA)
    }
  }

  return(list(model_results = model_results, sim_transformed = sim_transformed))
}
