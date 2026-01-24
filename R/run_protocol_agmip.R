#' @title Automate the AgMIP Phase IV Calibration protocol
#'
#' @inheritParams estim_param
#'
#' @param obs_list List of observed values to use in the protocol, in `cropr` format.
#' A named list (names = situation names) of data.frames, each containing:
#' - one column named `Date` with the observation dates (in `Date` or `POSIXct` format),
#' - and one column per observed variable, containing either the measured values or `NA`
#'   if the variable is not observed at the given date.
#'
#' @param optim_options (optional) List of options controlling the minimization method
#' (Nelder–Mead simplex), containing:
#' - `ranseed`: random seed used to make results reproducible. If `NULL`, the seed is not
#'   fixed and results may differ between runs. Otherwise, set it to an integer value
#'   (e.g. `1234`). Default is `NULL`.
#' - `nb_rep`: number of multi-start repetitions of the minimization.
#'   If provided, this value is used for all minimizations in both step6 and step7.
#'   The same setting is therefore applied to the whole protocol.
#'   By default, this is set to `c(10, 5)` for step6 (respectively for major-parameter estimation
#'   and for each candidate-parameter addition) and to `c(20)` for step7.
#' - `maxeval`: maximum number of evaluations of the minimized criterion per minimization
#'   (default: `50000`).
#' - `xtol_rel`: relative tolerance threshold on parameter values. Minimization stops when
#'   parameter values change by less than `xtol_rel` times the absolute value of the parameter
#'   between two successive iterations (default: `1e-4`).
#' - additional options can be provided; see `?nl.opts` for a complete list.
#'
#' @param param_info Information about the parameters to estimate.
#' A list containing:
#' - `ub` and `lb`: named numeric vectors of upper and lower bounds,
#' - `default`: named numeric vector of default values (optional).
#'
#' The names correspond to the parameter names.
#' Default values are used when a parameter is not estimated in the current step
#' (e.g. major or candidate parameter estimated in a subsequent step, candidate parameter
#' that was not selected, etc.), and also as one of the initial values when the parameter is estimated.
#'
#' @param forced_param_values Named vector or list specifying parameter values to force in
#' the model.
#' It may also contain arithmetic expressions to define equality constraints between parameters
#' (see the Details section of `estim_param`). In this case, the values to force are computed before
#' each call to the model wrapper and passed through its `param_values` argument during the estimation procedure.
#' This argument should not include values for parameters that are estimated (i.e. parameters
#' defined in `param_info`).
#'
#' @param transform_obs User-defined function to transform observations before each criterion
#' evaluation (optional). See the Details section of `estim_param` for more information.
#'
#' @param transform_sim User-defined function to transform simulations before each criterion
#' evaluation (optional). See the Details section of `estim_param` for more information.
#'
#' @param satisfy_par_const User-defined function to enforce inequality constraints on estimated
#' parameters (optional). See the Details section of `estim_param` for more information.
#'
#' @param info_crit_func Function or list of functions used to compute information criteria
#' (optional; see the default value in the function signature and
#' https://sticsrpacks.github.io/CroptimizR/reference/information_criteria.html
#' for the list of available criteria).
#'
#' The values of all provided information criteria are stored in the returned object.
#' If parameter selection is activated (i.e. if `candidate_param` is provided in at least one
#' step of step6), the first information criterion in the list is used for parameter selection.
#'
#' @param step A list defining the sub-steps for step 6 of the AgMIP Calibration protocol (see Details section).
#'
#' @details
#'
#' ## The AgMIP Phase IV Calibration protocol
#'
#' The AgMIP Phase IV Calibration protocol is thoroughly described in Wallach et al. (2024)
#' and Wallach et al. (2025).
#'
#' This protocol consists of two successive steps, called step6 and step7.
#'
#' Step6 consists in a sequential parameter estimation by groups of variables.
#' For each group of variables, parameters are estimated by ordinary least squares (OLS)
#' using a multi-start Nelder–Mead optimization (i.e. several minimizations starting from
#' different initial values).
#' Once estimated, parameters are fixed to their estimated values for the subsequent steps.
#'
#' For each group of variables, the user defines:
#' - a set of *major parameters*, supposed to mainly reduce bias for these variables,
#' - and a set of *candidate parameters*, expected to explain variability between environments.
#'
#' Candidate parameters should be ordered, as far as possible, by decreasing expected importance.
#' For each group of variables, candidate parameters are progressively added to the list of
#' parameters to estimate, and are retained only if they improve an information criterion
#' (corrected Akaike Information Criterion by default).
#' If a candidate parameter is not selected, it is fixed to its default value.
#'
#' By default, the estimation of the major parameters for a given step is performed using
#' 10 multi-start repetitions. When candidate parameters are considered, 5 additional
#' multi-start repetitions are performed each time a new candidate parameter is added to
#' the set of parameters to estimate.
#'
#' Step7 consists in re-estimating all parameters selected during step6 using all available
#' observations, by weighted least squares (WLS). The weights are set to the estimated standard
#' deviation of the model error for each variable, as obtained at the end of step6.
#'
#' The WLS minimization is performed using a multi-start Nelder–Mead simplex algorithm with
#' 20 repetitions by default. The first two repetitions are initialized respectively from:
#' (i) the parameter values estimated at the end of step6, and
#' (ii) the default parameter values.
#' The remaining repetitions are initialized from parameter values randomly drawn within
#' their respective bounds.
#'
#' ## Describing step6 (argument `step`)
#'
#' The argument `step` is a list of lists describing the successive sub-steps to apply in step6
#' of the AgMIP protocol. Each sub-step corresponds to a group of variables (e.g. phenology,
#' biomass, etc.).
#'
#' Each element of `step` is a named list that must contain:
#' - `obs_var`: a character vector giving the names of the observed variables to use at this step,
#' - optionally, `major_param`: a character vector giving the names of the major parameters to estimate at this step,
#' - optionally, `candidate_param`: a character vector giving the names of the candidate parameters.
#'
#' At least one of `major_param` or `candidate_param` must be provided.
#' If `candidate_param` is not provided, only the major parameters are estimated for this step.
#' If `major_param` is not provided, the step only performs candidate-parameter selection.
#'
#' The name of each list element is optional, but it is recommended to use the name of the
#' corresponding group of variables. This name is used in printed outputs and in the results.
#'
#' Technical information about parameters (bounds, default values, ...), the observation list
#' in `cropr` format, optimization options, forced parameter values, transformation functions,
#' functions defining equality constraints, the information criterion function, etc., can be
#' provided **once for all steps** via the corresponding arguments of `run_protocol_agmip`
#' (`param_info`, `obs_list`, ...).
#'
#' If the user wants this information to be specific to a given step, it can also be provided
#' inside the corresponding step description, using the same argument names.
#'
#' For example:
#'
#' ```r
#' param_info <- list(
#'   p1 = list(lb = 0, ub = 1, default = 0.1),
#'   p2 = list(lb = 0, ub = 1, default = 0.5),
#'   p3 = list(lb = 5, ub = 15, default = 15)
#' )
#'
#' steps <- list(
#'   group1 = list(
#'     obs_var = c("var1", "var2"),
#'     major_param = c("p1"),
#'     candidate_param = c("p2")
#'   ),
#'   group2 = list(
#'     obs_var = c("var3"),
#'     major_param = c("p3")
#'   )
#' )
#'
#' res <- run_protocol_agmip(
#'   obs_list = obs_list,
#'   model_function = my_model_wrapper,
#'   model_options = model_options,
#'   param_info = param_info,
#'   step = steps
#' )
#' ```
#'
#' In this example, step6 of the AgMIP protocol will be run in two successive steps called
#' "group1" and "group2".
#' In the first step, variables "var1" and "var2" are used to estimate parameter "p1"
#' (major parameter), and candidate parameter "p2" is considered in the automatic parameter
#' selection procedure.
#' The observations for variables "var1" and "var2", as well as the information about
#' parameters "p1" and "p2", are automatically extracted from `obs_list` and `param_info`,
#' which contain the information for all steps.
#'
#' ## Observations used in step7
#'
#' Note that in step7, all observations included in `obs_list` are used, regardless of the
#' `obs_var` variables defined for step6.
#' Thus, observations for variables not used in step6 (e.g. because no parameter is directly
#' associated with these variables) can still be used in step7, where all parameters selected
#' during step6 are re-estimated using all observed variables (WLS step).
#'
#' @return
#' Prints, graphs, and a list containing the results of the AgMIP Phase IV Calibration protocol.
#' All results are saved in the folder specified by `out_dir`.
#'
#' During execution, a console display indicates the description of the step currently being run.
#'
#' The generated plots include:
#' - diagnostics recommended in Wallach et al. (2025): MSE, bias², rRMSE, and Efficiency
#'   for each variable at each step (files `barplot_rRMSE_EF_per_step.pdf` and
#'   `plot_MSE_Bias2_per_step.pdf`),
#' - scatter plots of simulations versus observations before and after each step
#'   (files `scatter_plots_*.pdf`),
#' - diagnostic plots for each minimization performed (see subfolders `AgMIP_protocol_step6`,
#'   `AgMIP_protocol_step7`, and their contents).
#'
#' The returned object is a list containing:
#' - `final_values`: a named vector with the estimated parameter values,
#' - `forced_param_values`: a named vector with the values of forced parameters,
#' - `obs_var_list`: a character vector with the names of observed variables used in the protocol,
#' - `values_per_step`: a data.frame containing the default parameter values (from `param_info$default`, or `NA` if not provided) and the estimated
#'   values after step6 and step7,
#' - `stats_per_step`: a data.frame containing statistics (MSE, bias², rRMSE, and Efficiency)
#'   for each variable, before and after each step,
#' - `step6`: a list with detailed results for step6,
#' - `step7`: a list with detailed results for step7.
#'
#'
#' @seealso
#'   * Wallach et al. (2024) and Wallach et al. (2025) for a detailed description of the AgMIP
#'     calibration protocol,
#'   * the `estim_param` function for basic parameter estimation using CroptimizR,
#'   * the `load_protocol_agmip` function to extract `step` and `param_info` from a structured
#'     Excel file.
#'
#' @importFrom CroPlotR save_plot_pdf
#' @importFrom dplyr bind_rows mutate
#'
#' @export
#'
run_protocol_agmip <- function(obs_list, model_function, model_options, optim_options = list(),
                               param_info = NULL, forced_param_values = NULL, transform_var = NULL,
                               transform_obs = NULL, transform_sim = NULL, satisfy_par_const = NULL,
                               var_to_simulate = NULL,
                               info_crit_func = list(
                                 CroptimizR::AICc,
                                 CroptimizR::AIC,
                                 CroptimizR::BIC
                               ),
                               step, out_dir = getwd(), info_level = 0) {
  res <- NULL
  optim_options_given <- optim_options
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  on.exit({
    save(res, file = file.path(out_dir, "optim_results.Rdata"))
    cat("\nEnd of AgMIP Phase IV protocol\n")
  })

  # Check input arguments
  if (!is.obs(obs_list)) {
    stop("Incorrect format for argument obs_list.")
  }
  if (is.null(step)) {
    stop("step argument must not be null.")
  } else {
    steps <- step
  }

  cat("\nAgMIP Calibration Phase IV protocol: automatic calculation steps 6 and 7",
      "\n(see doi.org/10.1016/j.envsoft.2024.106147 for a detailed description of the full protocol)\n",
      sep = ""
  )

  # Prefix the steps name by "Step6." for a clearer display of the steps names.
  # In case the step has no name, names them Step6.Group1, Step6.Group2, ...
  if (is.null(names(steps))) {
    names(steps) <- paste0("Group", seq(steps))
  }
  names(steps) <- paste0("Step6.", names(steps))

  # Run model wrapper using the default values of the parameters
  tmp <- compute_simulations(
    model_function = model_function,
    model_options = model_options,
    param_values = c(
      get_params_default(param_info),
      compute_eq_const(forced_param_values, get_params_default(param_info))
    ),
    situation = names(obs_list),
    var_to_simulate = var_to_simulate, obs_list = obs_list,
    transform_sim = transform_sim, transform_var = transform_var
  )
  if (!is.null(tmp$sim_transformed)) {
    sim_default <- tmp$sim_transformed
  } else {
    sim_default <- tmp$model_results
  }
  # Transform obs if specified
  obs_transformed <- obs_list
  if (!is.null(transform_obs)) {
    obs_transformed <- tryCatch(
      transform_obs(
        model_results = sim_default, obs_list = obs_list,
        param_values = NULL,
        model_options = model_options
      ),
      error = function(cond) {
        message(paste("Caught an error while executing the user function for transforming
        observations (argument transform_obs)."))
        print(cond)
        stop()
      }
    )
  }
  # Transform obs var if specified
  if (!is.null(transform_var)) {
    obs_transformed <- lapply(obs_transformed, function(x) {
      for (var in intersect(names(x), names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
  }

  # Compute stats for default values of the parameters
  stats_default <- summary(sim_default$sim_list,
                           obs = obs_transformed, stats = c("Bias2", "MSE", "rRMSE", "EF")
  ) %>%
    dplyr::mutate(step = "Default") %>%
    dplyr::select(step, dplyr::everything(), -group, -situation)
  # Generate scatter plots from default values of parameters
  p_default <- plot(sim_default$sim_list, obs = obs_transformed, type = "scatter")
  # Save the plots in the out_dir
  save_plot_pdf(
    p_default,
    out_dir = out_dir,
    file_name = "scatter_plots_default"
  )


  # Run step6
  cat("\n", make_display_prefix(1, "title"), "Step6\n", sep = "")
  # Force nb_rep to c(10, 5) for step6 as defined in the AgMIP Phase IV protocol
  if (is.null(optim_options_given$nb_rep)) {
    optim_options$nb_rep <- c(10, 5)
  }

  # Define initial values for step6 if not done by the user:
  # default values for 1st rep., the values for the other rep. and randomly sampled in estim_param
  if (is.null(get_init_values(param_info))) {
    default <- get_params_default(param_info)
    if (!is.null(default)) {
      param_info <- set_init_values(
        param_info,
        dplyr::bind_rows(default)
      )
    }
  }

  res_step6 <- estim_param(
    obs_list = obs_list,
    model_function = model_function,
    model_options = model_options,
    crit_function = crit_ols,
    optim_options = optim_options,
    param_info = param_info,
    forced_param_values = forced_param_values,
    transform_var = transform_var,
    transform_obs = transform_obs,
    transform_sim = transform_sim,
    satisfy_par_const = satisfy_par_const,
    var_to_simulate = var_to_simulate,
    info_crit_func = info_crit_func,
    step = steps,
    out_dir = file.path(out_dir, "AgMIP_protocol_step6"),
    info_level = info_level
  )
  if (length(steps) == 1) {
    res_step6[[names(steps)]] <- res_step6
  }

  # Run the model with the parameters values estimated at each sub-step of step 6
  stats_step6 <- list()
  p_step6 <- list()
  for (istep in seq(steps)) {
    ## Run the model for the current step
    step_name <- names(steps)[istep]
    tmp <- compute_simulations(
      model_function = model_function,
      model_options = model_options,
      param_values = c(
        res_step6[[step_name]]$final_values,
        res_step6[[step_name]]$forced_param_values
      ),
      situation = names(obs_list),
      var_to_simulate = var_to_simulate, obs_list = obs_list,
      transform_sim = transform_sim, transform_var = transform_var
    )
    if (!is.null(tmp$sim_transformed)) {
      sim <- tmp$sim_transformed
    } else {
      sim <- tmp$model_results
    }

    # Compute stats
    stats_step6[[istep]] <- summary(
      sim$sim_list,
      obs = obs_transformed, stats = c("Bias2", "MSE", "rRMSE", "EF")
    ) %>%
      dplyr::mutate(step = names(steps)[istep]) %>%
      dplyr::select(step, dplyr::everything(), -group, -situation)

    # Generate scatter plots for each sub-step of step6
    p_step6[[istep]] <- plot(sim$sim_list, obs = obs_transformed, type = "scatter")
    # Save the plots in the out_dir
    save_plot_pdf(
      p_step6[[istep]],
      out_dir = out_dir,
      file_name = paste0("scatter_plots_after_step6.", names(steps)[istep])
    )
  }
  stats_step6 <- dplyr::bind_rows(stats_step6)

  cat("\n\n", make_display_prefix(1, "title"), "End of Step6\n", sep = "")


  # Compute weights for step7

  ## Compute SSE and number of observations for each observed variable
  stats_tmp <- summary(sim$sim_list,
                       obs = obs_transformed, stats = c("n_obs", "SS_res")
  )

  ## Sum SSE and number of observations per group of variables
  ## (weight of variables belonging to the same group must be the same)
  ### Compute the list of variables used at the same step for each variable
  obs_var_names <- get_obs_var(obs_transformed)
  step_var_map <- lapply(obs_var_names, function(var) {
    unique(
      unlist(
        lapply(res_step6[names(steps)], function(x) {
          if (var %in% x$obs_var) x$obs_var else NULL
        })
      )
    )
  })
  names(step_var_map) <- obs_var_names
  SSE <- compute_stat_per_group("SS_res", step_var_map, stats_tmp)
  n_obs <- compute_stat_per_group("n_obs", step_var_map, stats_tmp)
  names(SSE) <- obs_var_names
  names(n_obs) <- obs_var_names

  ## Compute number of estimated parameters per variable
  p <- vapply(obs_var_names, function(var) {
    sum(sapply(res_step6[names(steps)], function(x) {
      if (var %in% x$obs_var) {
        return(length(x$final_values))
      } else {
        return(0)
      }
    }))
  }, numeric(1))
  names(p) <- obs_var_names

  ## Define weight function
  weights <- vapply(obs_var_names, function(var) {
    if (n_obs[[var]] == p[[var]]) {
      warning(paste(
        "Number of observations for variable", var,
        "is equal to the number of parameters (", p[[var]],
        "). This lead to an infinite weight for step7.\n",
        "This variable will thus not be taken into account in step7."
      ))
    }
    sqrt(SSE[[var]] / (n_obs[[var]] - p[[var]]))
  }, numeric(1))
  names(weights) <- obs_var_names

  weight_step7 <- function(obs, var) {
    return(weights[[var]])
  }

  # Define parameters to estimate and to set for step7
  param_info_step7 <- filter_param_info(param_info, names(res_step6$final_values))
  forced_param_values_step7 <- NULL
  if (length(res_step6$forced_param_values) > 0) {
    forced_param_values_step7 <- res_step6$forced_param_values
  }

  # Define initial values for step7: values estimated at step6 for 1st rep.,
  # default values for 2nd rep., the values for the other rep. and randomly sampled
  param_info_step7 <- set_init_values(
    param_info_step7,
    dplyr::bind_rows(
      as.data.frame(t(res_step6$final_values)),
      get_params_default(param_info_step7)
    )
  )

  # Force nb_rep to 20 for step7 as defined in the AgMIP Phase IV protocol
  if (is.null(optim_options_given$nb_rep)) {
    optim_options$nb_rep <- 20
  }

  # Run step7
  cat("\n", make_display_prefix(1, "title"), "Step7\n", sep = "")
  res_step7 <- estim_param(
    obs_list = obs_list,
    model_function = model_function,
    model_options = model_options,
    crit_function = crit_wls,
    optim_options = optim_options,
    param_info = param_info_step7,
    forced_param_values = forced_param_values_step7,
    transform_var = transform_var,
    transform_obs = transform_obs,
    transform_sim = transform_sim,
    satisfy_par_const = satisfy_par_const,
    var_to_simulate = var_to_simulate,
    weight = weight_step7,
    out_dir = file.path(out_dir, "AgMIP_protocol_step7"),
    info_level = info_level
  )

  # Compute diagnostics

  ## Run model wrapper using parameter values estimated in step7
  tmp <- compute_simulations(
    model_function = model_function,
    model_options = model_options,
    param_values = c(
      res_step7$final_values,
      res_step7$forced_param_values
    ),
    situation = names(obs_list),
    var_to_simulate = var_to_simulate, obs_list = obs_list,
    transform_sim = transform_sim, transform_var = transform_var
  )
  if (!is.null(tmp$sim_transformed)) {
    sim_after_step7 <- tmp$sim_transformed
  } else {
    sim_after_step7 <- tmp$model_results
  }
  ## Compute goodness-of-fit stats after step7
  stats_step7 <- summary(sim_after_step7$sim_list,
                         obs = obs_transformed, stats = c("Bias2", "MSE", "rRMSE", "EF")
  ) %>%
    dplyr::mutate(step = "Step7") %>%
    dplyr::select(step, dplyr::everything(), -group, -situation)

  ## Generate scatter plots from default values of parameters, estimated values after step6 and estimated values after step7
  p_step7 <- plot(sim_after_step7$sim_list, obs = obs_transformed, type = "scatter")
  ## Save the plots in the out_dir
  save_plot_pdf(
    p_step7,
    out_dir = out_dir,
    file_name = "scatter_plots_after_step7"
  )

  ## Concatenate the different stats in a data.frame with a column variable, a column step and a column per stat
  stats_per_step <- dplyr::bind_rows(stats_default, stats_step6, stats_step7)
  ## Arrange it per variable name
  stats_per_step <- stats_per_step[order(stats_per_step$variable), ]

  # Plot diagnostics graphs
  ## Evolution of MSE and Bias2 for all steps and variables
  steps_by_var_tmp <- lapply(names(steps), function(step_name) {
    vars <- res_step6[[step_name]]$obs_var
    setNames(rep(step_name, length(vars)), vars)
  })
  steps_by_var_all <- unlist(steps_by_var_tmp)
  # Keep the last occurence of each variable (i.e., the step where it was last used)
  steps_by_var <- steps_by_var_all[!duplicated(names(steps_by_var_all), fromLast = TRUE)]
  p_evol <- plot_stats_evolution(stats_per_step, steps_by_var)
  ggsave(
    filename = file.path(out_dir, "plot_MSE_Bias2_per_step.pdf")
  )
  ## Bar graphs of rRMSE and EF for all variables and main steps (default, step6, step7)
  p_bar <- plot_stats_bars(stats_per_step)
  ggsave(
    filename = file.path(out_dir, "barplot_rRMSE_EF_per_step.pdf"),
    width = 11, height = 8.5, units = "in"
  )

  cat("\n\n", make_display_prefix(1, "title"), "End of Step7\n", sep = "")

  # Print results
  cat(
    "\nGraphs and results can be found in ",
    out_dir, "\n",
    sep = ""
  )

  # Return results
  res$final_values <- res_step7$final_values
  res$forced_param_values <- res_step7$forced_param_values
  res$obs_var_list <- res_step7$obs_var_list
  default_values <- get_params_default(param_info)
  res$values_per_step <-
    data.frame(
      name = names(res_step7$final_values),
      default = sapply(names(res_step7$final_values), function(x) {
        if (x %in% names(default_values)) {
          return(default_values[[x]])
        } else {
          return(NA)
        }
      }),
      step6 = res_step6$final_values,
      step7 = res_step7$final_values
    )
  res$stats_per_step <- stats_per_step
  res$step6 <- res_step6
  res$step7 <- res_step7
  res$step7$weights <- data.frame(
    variable = names(weights),
    weight = weights,
    SSE = SSE,
    n = n_obs,
    p = p
  )
  return(res)
}


#' @title Compute statistics per group of variables
#'
#' @param stat_col Name of the column of stats_per_var containing the statistics to compute
#'
#' @param step_var_map A list mapping each variable to a list of variables used at the same step
#'
#' @param stats_per_var A data frame containing statistics per variable
#'
#' @return A named vector containing for each variable the sum of the statistics for each group of variables
#'
#' @keywords internal
#'
compute_stat_per_group <- function(stat_col, step_var_map, stats_per_var) {
  if (is.null(names(stats_per_var)) || length(names(stats_per_var)) == 0) {
    stop(paste(
      "stats_per_var must be a data frame with named columns. \n While str(stats_per_var)=",
      paste(str(stats_per_var), collapse = ", ")
    ))
  }
  if (!stat_col %in% names(stats_per_var)) {
    stop(paste(
      stat_col, "not a column of stats_per_var. \nColumns of stats_per_var are:",
      paste(names(stats_per_var), collapse = ",")
    ))
  }

  vapply(
    names(step_var_map),
    function(var) {
      step_var_list <- step_var_map[[var]]

      if (is.null(step_var_list)) {
        # in case the variable is not used in step6 but is in step7, just take the stat of the variable
        stats_per_var[[stat_col]][which(stats_per_var$variable == var)]
      } else {
        # sum stats_per_var$stat_col of each variable belonging to the same step as var (as defined in res_step6$step$var)
        sum(stats_per_var[[stat_col]][stats_per_var$variable %in% step_var_list], na.rm = TRUE)
      }
    },
    numeric(1)
  )
}
