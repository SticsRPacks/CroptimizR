#' @title main function for criterion to optimize
#' @param param_values Value(s) of the parameters
#' @param crit_options A list containing the following elements:
#' `param_names` Name(s) of parameters
#' `obs_list` List of observed values
#' `crit_function` Function implementing the criterion to optimize
#' `model_function` Function implementing the criterion to optimize
#' `model_options` List of arguments to pass to model function
#' `situation_names` Name(s) of the situations to simulate
#' `param_info` Information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing a vector of upper and lower
#' bounds (`ub` and `lb`), or a list of list containing for each
#' parameter and group of situation the names of the situations (`sit_names`)
#' and upper and lower bounds (`ub` and `lb`)
#'
#' @return The value of the criterion
#'
#' @keywords internal
#'
main_crit <- function(param_values, crit_options) {
  on.exit({
    if (exists("obs_sim_list") && !is.na(obs_sim_list) &&
      is.list(obs_sim_list) && "obs_list" %in% names(obs_sim_list)) {
      .croptEnv$obs_var_list <- unique(
        unlist(lapply(
          obs_sim_list$obs_list,
          function(x) setdiff(names(x), c("Date"))
        ))
      )
      .croptEnv$obs_situation_list <- names(obs_sim_list$obs_list)
    }

    if (crit_options$info_level >= 1 && !is.null(crit_options$tot_max_eval)) {
      satisfy_const <- TRUE
      if (exists("flag_const")) satisfy_const <- flag_const

      if (is.null(.croptEnv$params_and_crit)) {
        .croptEnv$params_and_crit <- vector("list", crit_options$tot_max_eval)
        .croptEnv$eval_count <- 1
      } else {
        .croptEnv$eval_count <- .croptEnv$eval_count + 1
      }

      .croptEnv$params_and_crit[[.croptEnv$eval_count]] <-
        dplyr::bind_cols(
          crit = crit, tibble::tibble(!!!param_values_ori),
          satisfy_const = satisfy_const
        )
      if (!is.null(forced_param_values)) {
        .croptEnv$params_and_crit[[.croptEnv$eval_count]] <-
          dplyr::bind_cols(
            .croptEnv$params_and_crit[[.croptEnv$eval_count]],
            forced_param_values = tibble::tibble(!!!forced_param_values)
          )
      }

      if (!is.null(crit_options$irep)) {
        ## this condition is there to detect frequentist methods ...
        ## should be changed for more robust test later ...

        if ((.croptEnv$eval_count == 1) ||
          (crit_options$irep > tail(.croptEnv$params_and_crit[[.croptEnv$eval_count - 1]]$rep, 1))) {
          eval <- 1
          iter <- NA
          .croptEnv$last_iter <- 0
          .croptEnv$last_crit <- crit
          if (!is.na(crit)) {
            iter <- 1
            .croptEnv$last_iter <- iter
          }
        } else {
          eval <- tail(.croptEnv$params_and_crit[[.croptEnv$eval_count - 1]]$eval, 1) + 1
          iter <- NA
          if (!is.na(crit) && (is.na(.croptEnv$last_crit) ||
            crit < .croptEnv$last_crit)) {
            ## in the if above, is.na(.croptEnv$last_crit) is there in case
            ## is.na(crit) at the first evaluation
            iter <- .croptEnv$last_iter + 1
            .croptEnv$last_iter <- iter
            .croptEnv$last_crit <- crit
          }
        }

        .croptEnv$params_and_crit[[.croptEnv$eval_count]] <-
          dplyr::bind_cols(
            tibble::tibble(
              rep = crit_options$irep,
              eval = eval,
              iter = iter
            ),
            .croptEnv$params_and_crit[[.croptEnv$eval_count]]
          )
      }
    }

    if (crit_options$info_level >= 2 && !is.null(crit_options$tot_max_eval)) {
      if (is.null(.croptEnv$sim_intersect)) {
        .croptEnv$sim_intersect <- vector("list", crit_options$tot_max_eval)
      }
      if (exists("obs_sim_list") && !is.na(obs_sim_list) &&
        is.list(obs_sim_list) && "sim_list" %in% names(obs_sim_list)) {
        .croptEnv$sim_intersect[[.croptEnv$eval_count]] <- obs_sim_list$sim_list
      }
    }

    if (crit_options$info_level >= 3 && !is.null(crit_options$tot_max_eval)) {
      if (is.null(.croptEnv$obs_intersect)) {
        .croptEnv$obs_intersect <- vector("list", crit_options$tot_max_eval)
      }
      if (exists("obs_sim_list") && !is.na(obs_sim_list) &&
        is.list(obs_sim_list) && "obs_list" %in% names(obs_sim_list)) {
        .croptEnv$obs_intersect[[.croptEnv$eval_count]] <- obs_sim_list$obs_list
      }
    }

    if (crit_options$info_level >= 4 && !is.null(crit_options$tot_max_eval)) {
      if (is.null(.croptEnv$sim)) {
        .croptEnv$sim <- vector("list", crit_options$tot_max_eval)
      }
      .croptEnv$sim[[.croptEnv$eval_count]] <- sim

      if (!is.null(crit_options$transform_sim)) {
        if (is.null(.croptEnv$sim_transformed)) {
          .croptEnv$sim_transformed <- vector("list", crit_options$tot_max_eval)
        }
        .croptEnv$sim_transformed[[.croptEnv$eval_count]] <- sim_transformed
      }
    }

    if (is.na(crit)) {
      filename <- file.path(
        crit_options$out_dir,
        paste0("debug_crit_NA.Rdata")
      )
      warning(paste(
        "The optimized criterion has taken the NA value. \n  * Parameter values, obs_list and model results will be saved in",
        filename, "for sake of debugging."
      ))
      # just warns in this case. The optimization method may handle the problem
      # which may happend only for certain parameter values ...).
      save(param_values, obs_list, model_results, file = filename)
    }

    if (!is.null(crit_options$return_detailed_info) && crit_options$return_detailed_info) {
      return(res <- list(
        crit = crit, obs_list = obs_list, sim = sim,
        sim_transformed = sim_transformed,
        sim_intersect = obs_sim_list$sim_list,
        obs_intersect = obs_sim_list$obs_list,
        forced_param_values = forced_param_values
      ))
    }
  })

  # Initializations
  obs_list <- crit_options$obs_list
  crit_function <- crit_options$crit_function
  model_function <- crit_options$model_function
  model_options <- crit_options$model_options
  param_info <- crit_options$param_info
  transform_var <- crit_options$transform_var
  transform_obs <- crit_options$transform_obs
  transform_sim <- crit_options$transform_sim
  satisfy_par_const <- crit_options$satisfy_par_const
  var_to_simulate <- crit_options$var_to_simulate
  forced_param_values <- crit_options$forced_param_values

  param_names <- get_params_names(param_info)
  names(param_values) <- param_names
  situation_names <- names(obs_list)
  param_names_sl <- get_params_names(param_info, short_list = TRUE)
  crit <- NA
  obs_sim_list <- NULL
  sim_transformed <- NULL
  model_results <- NULL
  sim <- NULL

  # Denormalize parameters
  # TO DO

  # Transform parameters
  # TO DO

  # Handle the case of group of parameters (i.e. different values depending on
  # the situations)
  param_values_ori <- param_values
  if ("sit_list" %in% names(param_info[[1]])) {
    sit_names_param_info <- unique(unlist(lapply(
      param_info,
      function(x) {
        unlist(x$sit_list)
      }
    )))
    param_values_df <- sapply(sit_names_param_info,
      function(x) get_params_per_sit(param_info, x, param_values),
      simplify = FALSE
    )
    param_values <- dplyr::bind_rows(param_values_df, .id = "situation")
  }

  # Handle the parameters to force
  forced_param_values <- compute_eq_const(forced_param_values, param_values)
  if (tibble::is_tibble(param_values)) {
    param_values <-
      dplyr::bind_cols(forced_param_values, param_values)
  } else if (!is.null(forced_param_values) & length(forced_param_values) > 0) {
    param_values <- c(forced_param_values, param_values)
  }

  # Apply constraints on the parameters
  if (!is.null(satisfy_par_const)) {
    flag_const <- tryCatch(
      satisfy_par_const(param_values = param_values, model_options = model_options),
      error = function(cond) {
        message(paste("Caught an error while executing the user function for including
        constraints on estimated parameters (argument satisfy_par_const of estim_param function). \n
                 param_values=", paste(param_values, collapse = ",")))
        print(cond)
        stop()
      }
    )
    if (!flag_const) {
      crit_type <- crit_function()
      if (grepl("ls", crit_type)) {
        return(Inf)
      } else if (grepl("log-likelihood", crit_type)) {
        return(-Inf)
      } else if (grepl("likelihood", crit_type)) {
        return(0)
      } else {
        warning(cat("Unknown type returned by crit_function when used without arguments:", crit_type, ". Should be \"ls\", \"log-likelihood\" or \"likelihood\".\n Contraints on parameters will not be taken into account."))
      }
    }
  }

  sit_names <- situation_names
  if (is.null(var_to_simulate)) {
    var_to_simulate <- setdiff(unique(unlist(lapply(obs_list, names))), "Date")
    sit_var_dates_mask <- obs_list
  } else {
    # use var_to_simulate as given by the user in argument of estim_param
    # set sit_var_dates_mask to NULL so that it can not be used in place of
    # var_to_simulate to select variables
    sit_var_dates_mask <- NULL
  }

  # Call model function
  tictoc::tic(quiet = TRUE)
  # hum, this test is a bit uggly but seems to be necessary
  # (wrappers may have or not the arguments situation and var or their old name
  # sit_names and var_names and they may have both in case the use deprecated
  #  arguments for sit_names and var_to_simulate ...
  if ((("situation" %in% names(formals(model_function))) |
    ("var" %in% names(formals(model_function)))) |
    !(("sit_names" %in% names(formals(model_function))) |
      ("var_names" %in% names(formals(model_function))))) {
    try(model_results <- model_function(
      model_options = model_options,
      param_values = param_values,
      situation = sit_names,
      var = var_to_simulate,
      sit_var_dates_mask = sit_var_dates_mask
    ))
  } else if (("sit_names" %in% names(formals(model_function))) |
    ("var_names" %in% names(formals(model_function)))) {
    try(model_results <- model_function(
      model_options = model_options,
      param_values = param_values,
      sit_names = sit_names,
      var_names = var_to_simulate,
      sit_var_dates_mask = sit_var_dates_mask
    ))
    lifecycle::deprecate_warn(
      "0.5.0", "model_function(sit_names)",
      "model_function(situation)"
    )
    lifecycle::deprecate_warn(
      "0.5.0", "model_function(var_names)",
      "model_function(var)"
    )
  }
  tictoc::toc(quiet = TRUE, log = TRUE)
  .croptEnv$total_eval_count <- .croptEnv$total_eval_count + 1
  sim <- model_results


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
    model_results <- tryCatch(
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
    sim_transformed <- model_results
  }
  if (!is.null(transform_var)) {
    model_results$sim_list <- lapply(model_results$sim_list, function(x) {
      for (var in intersect(names(x), names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
    attr(model_results$sim_list, "class") <- "cropr_simulation"
    sim_transformed <- model_results
  }
  # Check results, return NA if incorrect
  if (is.null(model_results) ||
    (!is.null(model_results$error) && model_results$error)) {
    warning("Error in transformation of simulation results.")
    return(NA)
  }
  if (is.null(model_results$sim_list) || length(model_results$sim_list) == 0) {
    warning("Transformation of simulation results returned an empty list!")
    return(NA)
  }
  if (!is.sim(model_results$sim_list)) {
    warning("Format of results returned by transformation of model results is incorrect!")
    return(NA)
  }


  # Transform observations
  if (!is.null(transform_obs)) {
    obs_list <- tryCatch(
      transform_obs(
        model_results = model_results, obs_list = obs_list,
        param_values = param_values,
        model_options = model_options
      ),
      error = function(cond) {
        message(paste("Caught an error while executing the user function for transforming
        observations (argument transform_obs of estim_param function). \n
                 param_values=", paste(param_values, collapse = ",")))
        print(cond)
        stop()
      }
    )
  }
  if (!is.null(transform_var)) {
    obs_list <- lapply(obs_list, function(x) {
      for (var in intersect(names(x), names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
  }
  # Check results, return NA if incorrect
  if (is.null(obs_list) || !is.obs(obs_list)) {
    warning("Transformation of observations returned an empty list or a list with an unexpected format.")
    return(NA)
  }

  # Make observations and simulations consistent if possible
  obs_sim_list <- make_obsSim_consistent(
    model_results$sim_list,
    obs_list
  )

  # Intersect sim and obs
  obs_sim_list <- intersect_sim_obs(
    obs_sim_list$sim_list,
    obs_sim_list$obs_list
  )
  if (!is.list(obs_sim_list)) {
    stop("Intersection of simulations and observations is empty (no date and/or variable in common)!")
  }

  # check presence of Inf/NA in simulated results where obs is not NA
  if (is_sim_inf_or_na(obs_sim_list$sim_list, obs_sim_list$obs_list, param_values)) {
    warning("\nThe optimized criterion is set to NA.\n")
    return(NA)
  }

  # Filter reserved columns that should not be taken into account in the computation of the criterion
  obs_sim_list$sim_list <- sapply(obs_sim_list$sim_list,
    function(x) {
      x[, !(names(x) %in% "Plant"),
        drop = FALSE
      ]
    },
    simplify = F
  )
  attr(obs_sim_list$sim_list, "class") <- "cropr_simulation"
  obs_sim_list$obs_list <- sapply(obs_sim_list$obs_list,
    function(x) {
      x[, !(names(x) %in% "Plant"),
        drop = FALSE
      ]
    },
    simplify = F
  )

  # Check consistency of observations and simulations
  check_obsSim_consistency(
    obs_sim_list$sim_list,
    obs_sim_list$obs_list
  )

  # Compute criterion value
  potential_arglist <- list(
    sim_list = obs_sim_list$sim_list,
    obs_list = obs_sim_list$obs_list,
    weight = crit_options$weight
  )

  # Test weight function is well defined
  if (!is.null(crit_options$weight)) {
    var_list_tmp <- names(obs_sim_list$obs_list[[1]])
    var_tmp <- setdiff(var_list_tmp, "Date")[1]
    obsvec_tmp <- obs_sim_list$obs_list[[1]][, var_tmp]
    tryCatch(
      w <- crit_options$weight(na.omit(obsvec_tmp), var_tmp),
      error = function(cond) {
        message(paste("Caught an error while testing argument weight: \n
                 it must be a function that takes 2 input arguments (vector of observed
                      values and name of corresponding variable)"))
        print(cond)
        stop()
      }
    )
    if (!is.numeric(w)) {
      stop("Caught an error while testing argument weight: \n
        it must be  function that returns a numeric value (or vector of).")
    }
    if (length(w) != 1 & length(w) != length(na.omit(obsvec_tmp))) {
      stop("Caught an error while testing argument weight: \n
        it must be a function that returns a single value or a vector of values of size the size of
             the vector of observed values given as first argument.")
    }
  }

  crit <- do.call(crit_function, args = potential_arglist[names(formals(crit_function))])
  if (is.nan(crit)) {
    warning(paste0("Optimized criterion returned NaN value for parameters: ", paste(names(param_values), collapse = " "), ", values: ", paste(param_values, collapse = " ")))
  }

  return(crit)
}

utils::globalVariables(c(".croptEnv"))
