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
#' @param var (optional) List of variables for which the model wrapper must return
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
#' @param var_names `r lifecycle::badge("deprecated")` `var_names` is no
#'   longer supported, use `var` instead.
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
                        candidate_param = NULL, transform_var = NULL, transform_obs = NULL,
                        transform_sim = NULL, satisfy_par_const = NULL,
                        var = NULL, info_level = 1,
                        info_crit_func = list(
                          CroptimizR::BIC, CroptimizR::AICc,
                          CroptimizR::AIC
                        ),
                        weight=NULL,
                        var_names = lifecycle::deprecated()) {

  # Managing parameter names changes between versions:
  if (rlang::has_name(optim_options, "path_results")) {
    lifecycle::deprecate_warn("0.5.0", "estim_param(optim_options = 'is deprecated, use `out_dir` instead of `path_results`')")
  } else if (rlang::has_name(optim_options, "out_dir")) {
    # Note: we add a test here again because it is potentially never given
    optim_options$path_results <- optim_options$out_dir # to remove when we update inside the function
  }
  if (lifecycle::is_present(var_names)) {
    lifecycle::deprecate_warn("0.5.0", "estim_param(var_names)", "estim_param(var)")
  } else {
    var_names <- var # to remove when we update inside the function
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

  # Remove CroptimizR environment before exiting and save stored results (even if the process crashes)
  on.exit({
    if (exists(".croptEnv")) {
      rm(".croptEnv")
    }
    save(res, file = file.path(path_results_ORI, "optim_results.Rdata"))
  })

  # Check inputs

  ## optim_options
  if (is.null(optim_options$path_results)) {
    optim_options$path_results <- getwd()
  }
  if (!dir.exists(optim_options$path_results)) {
    dir.create(optim_options$path_results,
      recursive = TRUE
    )
  }
  path_results_ORI <- optim_options$path_results

  ## obs_list
  if (!is.obs(obs_list)) {
    stop("Incorrect format for argument obs_list.")
  }
  ## crit_function
  if (!is.function(crit_function)) {
    stop("Incorrect format for argument crit_function. Should be a function.")
  }
  ## model_function
  if (!is.function(model_function)) {
    stop("Incorrect format for argument model_function. Should be a function.")
  }
  ## optim_method
  if (!is.character(optim_method)) {
    stop("Incorrect format for argument optim_method. Should be of type character and contains the name of the parameter estimation method to use.")
  }
  ## param_info
  if (!is.list(param_info)) {
    stop("Incorrect format for argument param_info. Should be a list.")
  } else if (!all(is.element(c("lb", "ub"), names(param_info))) &&
    !all(sapply(param_info, function(x) all(is.element(c("lb", "ub"), names(x)))))) {
    stop("Incorrect format for argument param_info. Should contains lb and ub vectors.")
  }
  if (any(sapply(param_info, function(x) is.element("sit_list", names(x))))) {
    if (!all(sapply(param_info, function(x) is.element("sit_list", names(x))))) {
      stop("sit_list is defined for at least one parameter in argument param_info but not for all.")
    }
    param_info <- lapply(param_info, function(x) {
      if (length(x$sit_list) > length(x$lb) & length(x$lb) == 1) x$lb <- rep(x$lb, length(x$sit_list))
      if (length(x$sit_list) > length(x$ub) & length(x$ub) == 1) x$ub <- rep(x$ub, length(x$sit_list))
      return(x)
    })
    param_info <- lapply(param_info, function(x) {
      if (length(x$sit_list) > length(x$lb) & length(x$lb) == 1) x$lb <- rep(x$lb, length(x$sit_list))
      if (length(x$sit_list) > length(x$ub) & length(x$ub) == 1) x$ub <- rep(x$ub, length(x$sit_list))
      return(x)
    })
  }
  param_names <- get_params_names(param_info, short_list = TRUE)

  ## forced_param_values
  if (!is.null(forced_param_values)) {
    if (!is.vector(forced_param_values)) {
      stop("Incorrect format for argument forced_param_values, should be a vector.")
    }
    if (any(names(forced_param_values) %in% setdiff(param_names, candidate_param))) {
      tmp <- intersect(names(forced_param_values), setdiff(param_names, candidate_param))
      warning(
        "The following parameters are defined both in forced_param_values and param_info
           arguments of estim_param function while they should not (a parameter cannot
           be both forced and estimated except if it is part of the `candidate` parameters):",
        paste(tmp,collapse = ","),
        "\n They will be removed from forced_param_values."
      )
      forced_param_values <-
        forced_param_values[setdiff(names(forced_param_values),
                                    setdiff(param_names, candidate_param))]
    }
  }

  ## Information criterion
  info_crit_list <- list()
  if (is.function(info_crit_func)) {
    info_crit_list <- list(info_crit_func)
  } else if (is.list(info_crit_func)) {
    info_crit_list <- info_crit_func
  } else if (!is.null(info_crit_func)) {
    stop("Argument info_crit_func should be NULL, a function or a list of functions.")
  }
  if (!is.null(info_crit_func)) {
    sapply(info_crit_list, function(x) {
      if ((!is.function(x)) || (is.null(x()$name))) {
        stop(paste(
          "info_crit_func argument may be badly defined:\n",
          "The information functions should return a named list including an element called name containing the name of the function when called without arguments."
        ))
      }
    })
    # set to NULL the info_crit that are not compatible with the crit_function used
    # info_crit_list[sapply(info_crit_list, function(x) {
    #   (x()$name == "AIC" || x()$name == "AICc" || x()$name == "BIC") && !identical(crit_function, crit_ols)
    # })] <- NULL
  }
  if (length(info_crit_list) == 0) info_crit_list <- NULL


  ## candidate_param
  ## candidate_param can only be used if info_crit_list[[1]] is provided and if it is compatible with the crit_function used
  if (!is.null(candidate_param) && is.null(info_crit_list)) {
    stop("The argument candidate_param can only be used if argument info_crit_list is provided and compatible with the crit_function used.")
  }
  if (!all(candidate_param %in% param_names)) {
    stop("Parameters included in argument candidate_param must be defined in param_info argument.")
  }

  ## weight
  if (!is.function(weight) && !is.null(weight)) {
    stop("Incorrect format for argument weight: should be a function or NULL.")
  }

  # Measured elapse time
  tictoc::tic.clearlog()
  tictoc::tic(quiet = TRUE)

  # set seed
  set.seed(optim_options$ranseed)

  # Initializations before parameter selection loop
  oblig_param_list <- setdiff(param_names, candidate_param)
  crt_candidates <- oblig_param_list
  if (length(crt_candidates) == 0) crt_candidates <- candidate_param[[1]] # in case there are only candidates ...
  count <- 1
  param_selection_steps<-NULL
  tmp <- optim_switch(optim_method=optim_method,optim_options=optim_options)

  # Parameter selection loop
  while (!is.null(crt_candidates)) {
    cat("\n---------------------\n")
    cat(paste("Estimated parameters:", paste(crt_candidates, collapse = " "), "\n"))
    cat("---------------------\n")

    ## Filter information about the parameters to estimate
    param_info_tmp <- filter_param_info(param_info, crt_candidates)
    bounds <- get_params_bounds(param_info_tmp)
    forced_param_values_tmp <- forced_param_values
    inter_forc_cand <- names(forced_param_values_tmp) %in% crt_candidates
    if (any(inter_forc_cand)) {
      forced_param_values_tmp <- forced_param_values[-which(inter_forc_cand)]
    }

    ## Initialize parameters
    ## nb_rep may be different for the different parameter selection steps
    ## ... quite ugly ... should be improved ...
    init_values_nb <- tmp$init_values_nb[min(length(tmp$init_values_nb), count)]
    param_info_tmp <- complete_init_values(param_info_tmp,
      nb_values = init_values_nb,
      satisfy_par_const = satisfy_par_const
    )
    ### Initialize already estimated parameters with the values leading to the best criterion obtained so far
    if (!is.null(param_selection_steps)) {
      ind_min_infocrit <- which.min(param_selection_steps[[info_crit_list[[1]]()$name]])
      best_final_values <- param_selection_steps$`Final values`[[ind_min_infocrit]]
      names(best_final_values) <- param_selection_steps$`Estimated parameters`[[ind_min_infocrit]]
      init_values <- get_init_values(param_info_tmp)
      init_values[, names(best_final_values)] <- as.data.frame(as.list(best_final_values))[rep(1, init_values_nb), , drop = FALSE]
      param_info_tmp <- set_init_values(param_info_tmp, init_values)
    }

    if (!is.null(optim_options$nb_rep)) {
      optim_options$nb_rep <- init_values_nb
    }

    ## Redefine path_result in case of several steps (results are stored in step_* sub-directories of optim_options$path_results)
    if (!is.null(candidate_param)) {
      path_results <- file.path(
        path_results_ORI, "results_all_steps",
        paste0("step_", count)
      )
      if (!dir.exists(path_results)) dir.create(path_results, recursive = TRUE)
      optim_options$path_results <- path_results
    }

    crit_options <- list(
      param_names = crt_candidates, obs_list = obs_list,
      crit_function = crit_function, model_function = model_function,
      model_options = model_options, param_info = param_info_tmp,
      transform_var = transform_var,
      transform_obs = transform_obs, transform_sim = transform_sim,
      satisfy_par_const = satisfy_par_const,
      path_results = optim_options$path_results,
      var_names = var_names,
      forced_param_values = forced_param_values_tmp,
      info_level = info_level,
      info_crit_list = info_crit_list,
      weight=weight
    )

    ## Run the estimation
    res_tmp <- optim_switch(
      optim_method = optim_method, optim_options = optim_options,
      param_info = param_info_tmp, crit_options = crit_options
    )

    ## In case no results, there was an error during the estimation process => stop
    if (length(res_tmp)==0) {
      stop("There was an error during the parameter estimation process.
           Please check warnings and messages displayed above and/or by running warnings().")
    }

    ## The following is done only if parameter selection is activated
    if (!is.null(candidate_param)) {

      ### Update results in param_selection_steps
      param_selection_steps <- post_treat_FwdRegAgMIP(
        res_tmp, crit_options,
        crt_candidates, param_selection_steps
      )

      ### Select the next list of candidate parameters
      res_select_param <- select_param_FwdRegAgMIP(
        oblig_param_list, candidate_param,
        crt_candidates,
        param_selection_steps[[info_crit_list[[1]]()$name]]
      )
      crt_candidates <- res_select_param$next_candidates
      if (res_select_param$selected) {
        res <- res_tmp
      }
      count <- count + 1
    } else {
      crt_candidates <- NULL
      res <- res_tmp
    }
  }

  # Print and store results of parameter estimation steps if parameter selection was activated
  if (!is.null(candidate_param)) {
    summary_FwdRegAgMIP(param_selection_steps, info_crit_list, path_results_ORI,
                        res)
    save_results_FwdRegAgMIP(param_selection_steps, path_results_ORI)
    res$param_selection_steps <- param_selection_steps
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
