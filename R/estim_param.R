#' @title main function for parameter estimation
#'
#' @param obs_list List of observed values to use for parameter estimation.
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' See details section for more information on the list of observations actually
#' used during the parameter estimation process.
#'
#' @param crit_function Function implementing the criterion to optimize
#' (optional, see default value in the function signature). See
#' [here](https://sticsrpacks.github.io/CroptimizR/reference/ls_criteria.html)
#' for more details about the list of proposed criteria.
#'
#' @param model_function Crop Model wrapper function to use.
#'
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#'
#' @param optim_method Name of the parameter estimation method to use (optional,
#' see default value in the function signature). For the moment, can be "simplex"
#' or "dreamzs". See [here](https://sticsrpacks.github.io/CroptimizR/articles/Available_parameter_estimation_algorithms.html)
#' for a brief description and references on the available methods.
#'
#' @param optim_options List of options of the parameter estimation method, containing:
#'   - `out_dir` Directory path where to write the optimization results (optional, default to `getwd()`)
#'   - `ranseed` Set random seed so that each execution of estim_param give the same
#'   results when using the same seed. If you want randomization, set it to NULL,
#'   otherwise set it to a number of your choice (e.g. 1234) (optional, default to NULL, which means random seed)
#'   - specific options depending on the method used. Click on the links to see examples with the [simplex](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html)
#' and [DreamZS](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_DREAM.html) methods.
#'   - `path_results` `r lifecycle::badge("deprecated")` `path_results` is no longer supported, use `out_dir` instead.
#'
#' @param param_info Information on the parameters to estimate.
#' Either
#' a list containing:
#'    - `ub` and `lb`, named vectors of upper and lower bounds (-Inf and Inf can be used if init_values is provided),
#'    - `default`, named vectors of default values (optional, corresponding parameters are set to their default value when the parameter is part of the `candidate_param` list and when it is not estimated)
#'    - `init_values`, a data.frame containing initial
#' values to test for the parameters (optional, if not provided, or if less values
#' than number of repetitions of the minimization are provided, the, or part
#' of the, initial values will be randomly generated using LHS sampling within
#' parameter bounds).
#'
#' or a named list containing for each parameter:
#'   - `sit_list`, list the groups of situations for which the current estimated
#'   parameter must take different values (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html)
#' for an example),
#'   - `ub` and `lb`, vectors of upper and lower bounds (one value per group),
#'   - `init_values`, the list of initial values per group  (data.frame, one column per group, optional).
#'   - `default`, vector of default values per group (optional, the parameter is set to its default value when it is part of the `candidate_param` list and when it is not estimated)
#'
#' @param forced_param_values Named vector or list, must contain the values (or
#' arithmetic expression, see details section) for the model parameters to force. The corresponding
#' values will be transferred to the model wrapper through its param_values argument
#' during the estimation process.
#' Should not include values for estimated parameters (i.e. parameters defined in
#' `param_info` argument), except if they are listed as candidate parameters (see
#' argument `candidate_param`).
#'
#' @param candidate_param Names of the parameters, among those defined in the argument param_info,
#' that must only be considered as candidate for parameter estimation (see details section).
#'
#' @param situation (optional) List of situations to take into account within obs_list.
#' situation = NULL means that all situations in obs_list will be used.
#'
#' @param var (optional) List of observed variables to take into account within obs_list.
#' var = NULL means that all variables in obs_list will be used.
#'
#' @param transform_var Named vector of functions to apply both on simulated and
#' observed variables. `transform_var=c(var1=log, var2=sqrt)` will for example
#' apply log-transformation on simulated and observed values of variable var1,
#' and square-root transformation on values of variable var2.
#'
#' @param transform_obs User function for transforming observations before each criterion
#' evaluation (optional), see details section for more information.
#'
#' @param transform_sim User function for transforming simulations before each criterion
#' evaluation  (optional), see details section for more information.
#'
#' @param satisfy_par_const User function for including constraints on estimated
#' parameters (optional), see details section for more information.
#'
#' @param var_to_simulate (optional) List of variables for which the model wrapper must return
#' results.
#' By default the wrapper is asked to simulate only the observed variables. However,
#' it may be useful to simulate also other variables, typically when transform_sim
#' and/or transform_obs functions are used. Note however that it is
#' active only if the model_function used handles this argument.
#' If it is the case, and if the var argument is provided, then the list of observations
#' used will be restricted to the list of variables given in the var argument,
#' plus the ones possibly computed by the transform_sim function.
#'
#' @param info_level (optional) Integer that controls the level of information returned and stored
#' by estim_param (in addition to the results automatically provided that depends on the method used).
#' Higher code give more details.
#'   - `0` to add nothing,
#'   - `1` to add criterion and parameters values, and constraint if satisfy_par_const is provided, for each evaluation
#' (element params_and_crit in the returned list),
#'   - `2` to add model results, after transformation if transform_sim is provided, and after intersection with observations,
#' i.e. as used to compute the criterion for each evaluation (element sim_intersect in the returned list),
#'   - `3` to add observations, after transformation if transform_obs is provided, and after intersection with simulations,
#' i.e. as used to compute the criterion for each evaluation (element obs_intersect in the returned list),
#'   - `4` to add all model wrapper results for each evaluation, and all transformations if transform_sim is provided.
#' (elements sim and sim_transformed in the returned list).
#'
#' @param info_crit_func Function (or list of functions) to compute information criteria.
#' (optional, see default value in the function signature and [here](https://sticsrpacks.github.io/CroptimizR/reference/information_criteria.html)
#' for more details about the list of proposed information criteria.).
#' Values of the information criteria will be stored in the returned list.
#' In case parameter selection is activated (i.e. if the argument candidate_param
#' is defined (see details section)), the first information criterion given will be used.
#' ONLY AVAILABLE FOR THE MOMENT FOR crit_function==crit_ols.
#'
#' @param weight Weights to use in the criterion to optimize. A function that takes in input a vector
#' of observed values and the name of the corresponding variable and that must return either a single value
#' for the weights for the given variable or a vector of values of length the length of the vector of observed values given in input.
#'
#' @param step (optional) List that describes the steps of the parameter estimation process (see details section).
#' If `NULL`, a single default step will be created using the `estim_param` arguments
#'
#' @details
#'   In CroptimizR, parameter estimation is based on the comparison between the values
#'   of the observed and simulated variables at corresponding dates. Only the situations,
#'   variables and dates common to both observations (provided in `obs_list` argument),
#'   and simulations returned by the wrapper used, will be taken into account in
#'   the parameter estimation process.
#'   In case where the value of an observed variable is NA for a given situation and
#'   date, it will not be taken into account. In case where the value of a simulated
#'   variable is NA (or Inf) for a given situation and date for which there is an
#'   observation, the optimized criterion will take the NA value, which may stop the
#'   process, and the user will be warned.
#'
#'   If the candidate_param argument is given, a parameter selection procedure following
#'   the AgMIP calibration phaseIII protocol will be performed:
#'   The candidate parameters are added one by one (in the given order) to the parameters
#'   that MUST be estimated (i.e. the one defined in param_info but not in candidate_param).
#'   Each time a new candidate is added:
#'    - the parameter estimation is performed and an information criterion is computed (see argument info_crit_func)
#'    - if the information criterion is inferior to all the ones obtained before,
#'      then the current candidate parameter is added to the list of parameters to estimate
#'
#'   The result includes a summary of all the steps (data.frame param_selection_steps).
#'
#'   The optional argument `transform_obs` must be a function with 4 arguments:
#'   - model_results: the list of simulated results returned by the mode_wrapper used
#'   - obs_list: the list of observations as given to estim_param function
#'   - param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'   - model_options: the list of model options as given to estim_param function
#'
#'   It must return a list of observations (same format as `obs_list` argument) that
#'   will be used to compute the criterion to optimize.
#'
#'   The optional argument `transform_sim` must be a function with 4 arguments:
#'   - model_results: the list of simulated results returned by the mode_wrapper used
#'   - obs_list: the list of observations as given to estim_param function
#'   - param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'   - model_options: the list of model options as given to estim_param function
#'
#'   It must return a list of simulated results (same format as this returned by the model wrapper used)
#'   that will be used to compute the criterion to optimize.
#'
#'   The optional argument `satisfy_par_const` must be a function with 2 arguments:
#'   - param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'   - model_options: the list of model options as given to estim_param function
#'
#'   It must return a logical indicating if the parameters values satisfies the constraints
#'   (freely defined by the user in the function body) or not.
#'
#'   The optional argument `forced_param_values` may contain arithmetic expressions to
#'   automatically compute the values of some parameters in function of the values of
#'   parameters that are estimated (equality constraints). For that, `forced_param_values`
#'   must be a named list. Arithmetic expressions must be R expressions given under the
#'   shape of character strings. For example:
#'
#'   forced_param_values = list(p1=5, p2=7, p3="5*p5+p6")
#'
#'   will pass to the model wrapper the value 5 for parameter p1, 7 for parameter p2,
#'   and will dynamically compute the value of p3 in function of the values of parameters
#'   p5 and p6 iteratively provided by the parameter estimation algorithm. In this example,
#'   the parameters p5 and p6 must thus be part of the list of parameters to estimate, i.e.
#'   described in the `param_info` argument.
#'
#'   The argument `step` is a list of lists used to perform parameter estimation in multiple sequential steps.
#'   If provided, each step represents a separate stage in the estimation process,
#'   allowing different configurations for each step (e.g., different sets of parameters to estimate,
#'   different observed variables, different situations, etc.).
#'
#'   When multiple steps are defined, the parameter values estimated in one step
#'   are used as fixed values in the subsequent step.
#'   Each step is a named list containing only the elements that vary between steps.
#'   **Any argument** of the `estim_param` function (e.g., `var`, `candidate_param` ...)
#'   can be defined within a step. Note that the element `param` can be used to define
#'   the list of parameters to estimate at a given step.
#'
#'   Any element not explicitly defined in a step will inherit its value
#'   from the corresponding argument of `estim_param` and remain identical across all steps.
#'   If `NULL`, a single step is created using the function arguments.
#'
#'   Suppose the `step` argument is defined as follows:
#'   ```r
#'   step <- list()
#'   step[[1]] <- list(
#'     param = c("p1"),
#'     candidate_param = c("p2"),
#'     var = c("var1")
#'   )
#'   step[[2]] <- list(
#'     param = c("p3"),
#'     var = c("var2")
#'   )
#'  ```
#'
#'  In this case, the parameter estimation process will proceed in **two steps**:
#'   - **Step 1**: Parameter `p1` is estimated, while `p2` is included in a parameter selection process.
#'     Only observed variable `var1` (from `obs_list` defined in argument of `estim_param`) is used.
#'   - **Step 2**: Parameter `p3` is estimated, and only observed variable `var2` is used.
#'     Parameters `p1` (and possibly `p2`, if selected) are fixed at the values estimated in Step 1.
#'  Information on the parameters to estimate (bounds, ...) can be defined within
#'  the same `param_info` list given in argument of `estim_param`.
#'
#'  The results of the parameter estimation process are stored in the folder `out_dir`,
#'  with a separate subfolder for each step.
#'
#' @return prints, graphs and a list containing the results of the parameter estimation,
#' which content depends on the method used and on the values of the `info_level` argument.
#' All results are saved in the folder `optim_options$out_dir`.
#'
#' @seealso For more details and examples, see the different vignettes in
#' [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#'
#' @export
#'
#'

estim_param <- function(obs_list, crit_function = crit_log_cwss, model_function,
                        model_options = NULL, optim_method = "nloptr.simplex",
                        optim_options = NULL, param_info, forced_param_values = NULL,
                        candidate_param = NULL, situation = NULL, var = NULL, transform_var = NULL, transform_obs = NULL,
                        transform_sim = NULL, satisfy_par_const = NULL,
                        var_to_simulate = NULL, info_level = 1,
                        info_crit_func = list(
                          CroptimizR::BIC, CroptimizR::AICc,
                          CroptimizR::AIC
                        ),
                        weight = NULL,
                        step = NULL) {
  # Managing parameter names changes between versions:
  if (rlang::has_name(optim_options, "path_results")) {
    lifecycle::deprecate_warn("0.5.0", "estim_param(optim_options = 'is deprecated, use `out_dir` instead of `path_results`')")
  } else if (rlang::has_name(optim_options, "out_dir")) {
    # Note: we add a test here again because it is potentially never given
    optim_options$path_results <- optim_options$out_dir # to remove when we update inside the function
  }

  # Initialize res
  res <- list()

  # Create an environment accessible by all functions for storing information during the estimation process
  parent <- eval(parse(text = ".GlobalEnv"))
  .croptEnv <- new.env(parent)
  assign(
    x = ".croptEnv",
    value = .croptEnv,
    pos = parent
  )
  .croptEnv$total_eval_count <- 0

  path_results_ORI <- getwd()

  # Remove CroptimizR environment before exiting and save stored results (even if the process crashes)
  on.exit({
    if (exists(".croptEnv")) {
      rm(".croptEnv")
    }
    save(res, file = file.path(path_results_ORI, "optim_results.Rdata"))
  })

  # Complete step and validate it
  nb_steps <- length(step)
  if (nb_steps == 0) {
    step <- list(list())
    nb_steps <- 1
  }
  if (!is.list(step)) {
    stop("Incorrect format for argument step. Should be a list.")
  }
  if (!all(sapply(step, function(x) is.list(x)))) {
    stop("Incorrect format for argument step. Should be a list of lists.")
  }
  ## Complete each element of `step` with the arguments from `estim_param`,
  ## adding only those that are not already present.
  step <- fill_step_info(step, mc = match.call())
  ## Validate the structure of each element of `step`
  step <- validate_steps(step)
  res <- list()

  # Measured elapse time
  tictoc::tic.clearlog()
  tictoc::tic(quiet = TRUE)

  # set seed
  set.seed(optim_options$ranseed)

  estimated_param_values <- NULL

  # Loop over the different steps
  for (istep in 1:nb_steps) {
    # cat("\n------\n")
    # cat(paste("Step", istep, "\n"))
    #    cat("------\n")

    path_results_ORI <- step[[istep]]$optim_options$path_results

    path_results_step <- file.path(
      path_results_ORI, paste0("step", istep)
    )

    # Filter observations if necessary
    if (!identical(step[[istep]]$var, NULL)) {
      step[[istep]]$obs_list <- filter_obs(step[[istep]]$obs_list,
        var = step[[istep]]$var,
        include = TRUE
      )
    }
    if (!identical(step[[istep]]$situation, NULL)) {
      step[[istep]]$obs_list <- filter_obs(step[[istep]]$obs_list,
        situation = step[[istep]]$situation,
        include = TRUE
      )
    }

    # Add already estimated parameters values and default values of non-estimated ones to forced_param_values
    default_values <- get_params_default(step[[istep]]$param_info)
    forced_param_values_istep <- c(
      step[[istep]]$forced_param_values,
      estimated_param_values,
      default_values[setdiff(
        names(default_values),
        names(estimated_param_values)
      )]
    )

    # Initializations before parameter selection loop
    oblig_param_list <- setdiff(step[[istep]]$param, step[[istep]]$candidate_param)
    if (length(oblig_param_list) == 0) {
      crt_candidates <- step[[istep]]$candidate_param[[1]] # in case there are only candidates ...
    } else {
      crt_candidates <- oblig_param_list
    }
    count <- 1
    param_selection_steps <- NULL
    method_description <- optim_switch(
      optim_method = step[[istep]]$optim_method,
      optim_options = step[[istep]]$optim_options
    )
    param_selection_activated <- !is.null(step[[istep]]$candidate_param)

    # Parameter selection loop
    while (!is.null(crt_candidates)) {
      ## Filter information about the parameters to estimate
      param_info_cur <- filter_param_info(step[[istep]]$param_info, crt_candidates)
      bounds <- get_params_bounds(param_info_cur)
      forced_param_values_cur <- forced_param_values_istep
      forced_param_values_cur <- forced_param_values_cur[!names(forced_param_values_cur) %in% crt_candidates]
      #      cat("\n\t---------------------\n")
      cat("\n---------------------\n")
      cat(paste("Step", istep, "\n"))
      if (param_selection_activated) {
        cat(paste("Parameter automatic selection process: step", count, "\n"))
      }
      cat(paste("Estimated parameters:", paste(crt_candidates, collapse = " "), "\n"))
      cat(paste("Forced parameters:", paste(names(forced_param_values_cur), forced_param_values_cur, sep = "=", collapse = ", ")), "\n")
      cat("---------------------\n")


      ## Initialize parameters
      ## nb_rep may be different for the different parameter selection steps
      ## ... quite ugly ... should be improved ...
      init_values_nb <- method_description$init_values_nb[min(length(method_description$init_values_nb), count)]
      param_info_cur <- complete_init_values(param_info_cur,
        nb_values = init_values_nb,
        satisfy_par_const = step[[istep]]$satisfy_par_const
      )
      ### Initialize already estimated parameters with the values leading to the best criterion obtained so far
      if (!is.null(param_selection_steps)) {
        ind_min_infocrit <- which.min(param_selection_steps[[step[[istep]]$info_crit_list[[1]]()$name]])
        best_final_values <- param_selection_steps$`Final values`[[ind_min_infocrit]]
        names(best_final_values) <- param_selection_steps$`Estimated parameters`[[ind_min_infocrit]]
        init_values <- get_init_values(param_info_cur)
        init_values[, names(best_final_values)] <- as.data.frame(as.list(best_final_values))[rep(1, init_values_nb), , drop = FALSE]
        param_info_cur <- set_init_values(param_info_cur, init_values)
      }

      if (!is.null(step[[istep]]$optim_options$nb_rep)) {
        step[[istep]]$optim_options$nb_rep <- init_values_nb
      }

      ## Redefine path_result in case of several parameter selection steps
      ## (results are stored in results_param_select/step_* sub-directories of optim_options$path_results)
      if (param_selection_activated) {
        path_results <- file.path(
          path_results_step, "results_param_select",
          paste0("param_select_step", count)
        )
      } else {
        path_results <- path_results_step
      }
      if (!dir.exists(path_results)) dir.create(path_results, recursive = TRUE)
      step[[istep]]$optim_options$path_results <- path_results

      crit_options <- list(
        param_names = crt_candidates,
        obs_list = step[[istep]]$obs_list,
        crit_function = step[[istep]]$crit_function,
        model_function = step[[istep]]$model_function,
        model_options = step[[istep]]$model_options,
        param_info = param_info_cur,
        transform_var = step[[istep]]$transform_var,
        transform_obs = step[[istep]]$transform_obs,
        transform_sim = step[[istep]]$transform_sim,
        satisfy_par_const = step[[istep]]$satisfy_par_const,
        path_results = step[[istep]]$optim_options$path_results,
        var_to_simulate = var_to_simulate,
        forced_param_values = forced_param_values_cur,
        info_level = step[[istep]]$info_level,
        info_crit_list = step[[istep]]$info_crit_list,
        weight = step[[istep]]$weight
      )

      ## Run the estimation
      res_tmp <- optim_switch(
        optim_method = step[[istep]]$optim_method,
        optim_options = step[[istep]]$optim_options,
        param_info = param_info_cur,
        crit_options = crit_options
      )

      ## In case no results, there was an error during the estimation process => stop
      if (length(res_tmp) == 0) {
        stop("There was an error during the parameter estimation process.
           Please check warnings and messages displayed above and/or by running warnings().")
      }

      ## If the parameter selection process is activated, compute the next candidate parameters to estimate
      if (param_selection_activated) {
        ### Update results in param_selection_steps
        param_selection_steps <- post_treat_FwdRegAgMIP(
          res_tmp, crit_options,
          crt_candidates, param_selection_steps
        )

        ### Select the next list of candidate parameters
        res_select_param <- select_param_FwdRegAgMIP(
          oblig_param_list, step[[istep]]$candidate_param,
          crt_candidates,
          param_selection_steps[[step[[istep]]$info_crit_list[[1]]()$name]]
        )
        crt_candidates <- res_select_param$next_candidates
        if (res_select_param$selected) {
          res[[istep]] <- res_tmp
        }
        count <- count + 1
      } else {
        crt_candidates <- NULL
        res[[istep]] <- res_tmp
      }
    } # End parameter selection loop

    # Print and store results of parameter estimation steps if parameter selection was activated
    if (param_selection_activated) {
      cat("----------------------\n")
      cat(paste("Step", istep, "\n"))
      cat("End of parameter selection process\n")
      cat("----------------------\n\n")

      summary_FwdRegAgMIP(
        param_selection_steps, step[[istep]]$info_crit_list, path_results_step,
        res
      )
      save_results_FwdRegAgMIP(param_selection_steps, path_results_step)
      res[[istep]]$param_selection_steps <- param_selection_steps
    }

    # Gather estimated values in a single vector for next steps
    estimated_param_values <- c(
      estimated_param_values,
      res[[istep]]$final_values
    )
  } # End loop over the different steps

  if (nb_steps > 1) {
    cat("\n----------------------\n")
    cat("End of multi-step parameter estimation process\n")
    cat("----------------------\n")

    res <- post_treat_multi_step(res)
    summary_multi_step(res, path_results_ORI)
  } else {
    res <- res[[1]]
  }

  # Measure elapse time
  log.lst <- tictoc::tic.log(format = FALSE)
  timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
  res$model_total_time <- as.numeric(format(sum(timings), scientific = FALSE, digits = 1, nsmall = 0))
  res$model_average_time <- as.numeric(format(mean(timings), scientific = FALSE, digits = 2, nsmall = 0))
  cat(paste(
    "Average time for the model to simulate all required situations:", res$model_average_time,
    "sec elapsed\n"
  ))
  res$total_eval_count <- .croptEnv$total_eval_count
  cat(paste("Total number of criterion evaluation:", res$total_eval_count, "\n"))
  cat(paste("Total time of model simulations:", res$model_total_time, "sec elapsed\n"))
  tictoc::tic.clearlog()
  tictoc::toc(quiet = TRUE, log = TRUE)
  log.lst <- tictoc::tic.log(format = FALSE)
  res$total_time <- as.numeric(format(log.lst[[1]]$toc - log.lst[[1]]$tic, scientific = FALSE, digits = 1, nsmall = 0))
  cat(paste("Total time of parameter estimation process:", res$total_time, "sec elapsed\n"))
  cat("----------------------\n")
  tictoc::tic.clearlog()

  return(res)
}

utils::globalVariables(c(".croptEnv"))


#' @title Fill the step information
#'
#' @inheritParams estim_param
#'
#' @param mc A matched call object from `match.call()` in `estim_param`,
#' containing the arguments explicitly provided by the user when calling `estim_param`.
#' This ensures that `fill_step_info` has access to the correct function call context.
#'
#' @return A list of step definitions provided by the user in the `estim_param` function call,
#' enriched with default values and additional arguments passed to `estim_param`.
#' The returned object is a list of lists, where each sublist contains the characteristics of a step.
#'
#' @keywords internal
fill_step_info <- function(step, mc) {
  mc <- mc[-1] # remove the function name
  args_given <- lapply(mc, eval, envir = parent.frame())
  args_default <- as.list(formals(estim_param))[-1] # [-1] Exclude step
  args_default <- lapply(args_default, function(arg) {
    if (!is.symbol(arg) || (is.symbol(arg) && deparse(arg) != "")) {
      tryCatch(
        eval(arg, envir = parent.frame()),
        error = function(e) arg # if eval fails, the non evaluated value is kept
      )
    } else {
      arg # keep as it if no default value
    }
  })
  args_total <- modifyList(args_default, args_given)
  args_total <- args_total[setdiff(names(args_total), "step")]
  step <- lapply(step, function(x) {
    for (arg_name in names(args_total)) {
      if (!arg_name %in% names(x) && arg_name %in% names(args_total)) {
        val <- args_total[[arg_name]]
        if (!missing(val)) {
          if (is.symbol(val) && identical(deparse(val), "NULL")) {
            val <- NULL
          }
          x[[arg_name]] <- val
        }
      }
    }
    if (!"param" %in% names(x)) x$param <- setdiff(get_params_names(x$param_info, short_list = TRUE), x$candidate_param)
    return(x)
  })
  return(step)
}


#' @title Validate the information for a given step
#'
#' @param step A list containing the characteristics of a given step.
#'
#' @param istep The index of the step in the list of steps.
#'
#' @return The validated step with possibly updated information.
#'
#' @keywords internal
validate_step <- function(step, istep) {
  ## optim_options
  if (is.null(step$optim_options$path_results)) {
    step$optim_options$path_results <- getwd()
  }
  if (!dir.exists(step$optim_options$path_results)) {
    dir.create(step$optim_options$path_results, recursive = TRUE)
  }

  ## obs_list
  if (!is.obs(step$obs_list)) {
    stop(paste("Step", istep, ": Incorrect format for argument obs_list."))
  }

  ## crit_function
  if (!is.function(step$crit_function)) {
    stop(paste("Step", istep, ": Incorrect format for argument crit_function. Should be a function."))
  }

  ## model_function
  if (!is.function(step$model_function)) {
    stop(paste("Step", istep, ": Incorrect format for argument model_function. Should be a function."))
  }

  ## optim_method
  if (!is.character(step$optim_method)) {
    stop(paste("Step", istep, ": Incorrect format for argument optim_method. Should be of type character and contain the name of the parameter estimation method to use."))
  }

  ## param_info
  if (!is.list(step$param_info)) {
    stop(paste("Step", istep, ": Incorrect format for argument param_info. Should be a list."))
  } else if (!all(is.element(c("lb", "ub"), names(step$param_info))) &&
    !all(sapply(step$param_info, function(x) all(is.element(c("lb", "ub"), names(x)))))) {
    stop(paste("Step", istep, ": Incorrect format for argument param_info. Should contain 'lb' and 'ub' vectors."))
  }

  ## Handling of `sit_list` in param_info
  if (any(sapply(step$param_info, function(x) is.element("sit_list", names(x))))) {
    if (!all(sapply(step$param_info, function(x) is.element("sit_list", names(x))))) {
      stop(paste("Step", istep, ": `sit_list` is defined for at least one parameter in param_info but not for all."))
    }

    step$param_info <- lapply(step$param_info, function(x) {
      if (length(x$sit_list) > length(x$lb) & length(x$lb) == 1) x$lb <- rep(x$lb, length(x$sit_list))
      if (length(x$sit_list) > length(x$ub) & length(x$ub) == 1) x$ub <- rep(x$ub, length(x$sit_list))
      return(x)
    })
  }

  ## forced_param_values
  if (!is.null(step$forced_param_values)) {
    if (!is.vector(step$forced_param_values)) {
      stop(paste("Step", istep, ": Incorrect format for argument forced_param_values, should be a vector."))
    }
    if (any(names(step$forced_param_values) %in% setdiff(step$param, step$candidate_param))) {
      tmp <- intersect(names(step$forced_param_values), setdiff(step$param, step$candidate_param))
      warning(paste(
        "Step", istep, ": The following parameters are defined both in forced_param_values and param_info",
        "arguments of estim_param function while they should not (a parameter cannot",
        "be both forced and estimated except if it is part of the `candidate` parameters):",
        paste(tmp, collapse = ","),
        "\n They will be removed from forced_param_values."
      ))
      step$forced_param_values <- step$forced_param_values[setdiff(
        names(step$forced_param_values),
        setdiff(step$param, step$candidate_param)
      )]
    }
  }

  ## Information criterion
  if (is.function(step$info_crit_func)) {
    step$info_crit_list <- list(step$info_crit_func)
  } else if (is.list(step$info_crit_func)) {
    step$info_crit_list <- step$info_crit_func
  } else if (!is.null(step$info_crit_func)) {
    stop(paste("Step", istep, ": Argument info_crit_func should be NULL, a function, or a list of functions."))
  }

  if (!is.null(step$info_crit_func)) {
    sapply(step$info_crit_list, function(x) {
      if ((!is.function(x)) || (is.null(x()$name))) {
        stop(paste(
          "Step", istep, ": info_crit_func argument may be badly defined:\n",
          "The information functions should return a named list including an element called 'name' containing the name of the function when called without arguments."
        ))
      }
    })
  }
  if (length(step$info_crit_list) == 0) step$info_crit_list <- NULL

  ## candidate_param
  if (!is.null(step$candidate_param) && is.null(step$info_crit_list)) {
    stop(paste("Step", istep, ": The argument candidate_param can only be used if info_crit_list is provided and compatible with crit_function."))
  }
  if (!all(step$candidate_param %in% get_params_names(step$param_info, short_list = TRUE))) {
    stop(paste("Step", istep, ": Parameters included in candidate_param must be defined in param_info."))
  }

  ## weight
  if (!is.function(step$weight) && !is.null(step$weight)) {
    stop(paste("Step", istep, ": Incorrect format for argument weight: should be a function or NULL."))
  }

  return(step)
}


#' @title Validate the information for all steps
#'
#' @param steps A list of steps, each containing the characteristics of a given step.
#'
#' @return The validated list of steps with possibly updated information.
#'
#' @keywords internal
validate_steps <- function(steps) {
  steps <- lapply(seq_along(steps), function(i) {
    validate_step(steps[[i]], i)
  })
  return(steps)
}
