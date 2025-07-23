#' @title Run the AgMIP PhaseIV protocol
#'
#' @inheritParams estim_param
#'
#' @param protocol_file_path Path to the Excel file describing the AgMIP PhaseIV protocol.
#'
#' @details
#'
#' The AgMIP PhaseIV protocol is thoroughly detailed in Wallach et al. (2024)
#' and Wallach et al. (2025).
#'
#' @return prints, graphs and a list containing the results of the AgMIP PhaseIV protocol.
#' All results are saved in the folder `out_dir`.
#'
#' @seealso Wallach et al. (2024), Wallach et al. (2025), and the `estim_param` function.
#'
#' @importFrom CroPlotR save_plot_pdf
#'
#' @export
#'
run_protocol_agmip <- function(model_function, model_options, obs_list, optim_options, protocol_file_path = NULL,
                               out_dir = getwd(), var_to_simulate = NULL, transform_sim = NULL,
                               transform_obs = NULL, transform_var = NULL, step = NULL, param_info = NULL) {
  res <- NULL
  on.exit({
    save(res, file = file.path(out_dir, "optim_results.Rdata"))
    cat("\n---------------------\n")
    cat(paste("End of AgMIP Phase IV protocol"))
    cat("\n---------------------\n")
  })

  cat("\n---------------------\n")
  cat(paste("AgMIP Phase IV protocol"))
  cat("\n---------------------\n")

  # Read the excel file describing the protocol to generate the step6 list if step is not provided
  if (is.null(step)) {
    tmp <- load_protocol_agmip(protocol_file_path, transform_outputs = transform_sim)
    steps <- tmp$step
    param_info <- tmp$param_info
  } else {
    steps <- step
  }

  # Run model wrapper using the default values of the parameters
  sim_default <- compute_simulations(
    model_function = model_function,
    model_options = model_options,
    param_values = get_params_default(param_info),
    situation = names(obs_list),
    var_to_simulate = var_to_simulate, obs_list = obs_list,
    transform_sim = transform_sim, transform_var = transform_var
  )

  # Force nb_rep to c(10, 5) for step6 as defined in the AgMIP Phase IV protocol
  if (is.null(optim_options$nb_rep)) {
    optim_options$nb_rep <- c(10, 5)
  }

  # Run step6
  cat("\n---------------------\n")
  cat(paste("Run AgMIP Phase IV protocol Step6"))
  res_step6 <- estim_param(
    obs_list = obs_list,
    model_function = model_function,
    model_options = model_options,
    crit_function = crit_ols,
    optim_options = optim_options,
    param_info = param_info,
    step = steps,
    out_dir = file.path(out_dir, "AgMIP_protocol_step6")
  )

  # Compute weights for step7

  ## Run model wrapper using parameter values estimated in step6
  sim_after_step6 <- compute_simulations(
    model_function = model_function,
    model_options = model_options,
    param_values = c(
      res_step6$final_values,
      res_step6$forced_param_values
    ),
    situation = names(obs_list),
    var_to_simulate = var_to_simulate, obs_list = obs_list,
    transform_sim = transform_sim, transform_var = transform_var
  )

  ## Transform observations if necessary
  obs_list_transformed <- obs_list
  if (!is.null(transform_obs)) {
    obs_list_transformed <- tryCatch(
      transform_obs(
        model_results = sim_after_step6$model_results, obs_list = obs_list,
        param_values = c(
          res_step6$final_values,
          res_step6$forced_param_values
        ),
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
  if (!is.null(transform_var)) {
    obs_list_transformed <- lapply(obs_list_transformed, function(x) {
      for (var in intersect(names(x), names(transform_var))) {
        x[var] <- transform_var[[var]](x[var])
      }
      return(x)
    })
  }

  ## Compute SSE and number of observations for each observed variable
  stats_tmp <- summary(sim_after_step6$model_results$sim_list,
    obs = obs_list_transformed, stats = c("n_obs", "SS_res")
  )

  ## Sum SSE and number of observations per group of variables
  ## (weight of variables belonging to the same group must be the same)
  ### Compute the list of variables used at the same step for each variable
  obs_var_names <- get_obs_var(obs_list_transformed)
  step_var_map <- lapply(obs_var_names, function(var) {
    unique(
      unlist(
        lapply(res_step6$step, function(x) {
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
    sum(sapply(res_step6$step, function(x) {
      if (var %in% x$obs_var) {
        return(length(x$optim_results$final_values))
      } else {
        return(0)
      }
    }))
  }, numeric(1))
  names(p) <- obs_var_names

  ## Define weight function
  weights <- vapply(obs_var_names, function(var) {
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

  # Define initial values for step7
  param_info_step7 <- set_init_values(
    param_info_step7,
    as.data.frame(t(res_step6$final_values))
  )

  # Force nb_rep to 20 for step7 as defined in the AgMIP Phase IV protocol
  if (is.null(optim_options$nb_rep)) {
    optim_options$nb_rep <- 20
  }

  # Run step7
  cat("\n---------------------\n")
  cat(paste("Run AgMIP Phase IV protocol Step7"))
  res_step7 <- estim_param(
    obs_list = obs_list,
    model_function = model_function,
    model_options = model_options,
    crit_function = crit_wls,
    optim_options = optim_options,
    param_info = param_info_step7,
    forced_param_values = forced_param_values_step7,
    weight = weight_step7,
    out_dir = file.path(out_dir, "AgMIP_protocol_step7")
  )

  # Compute diagnostics

  ## Run model wrapper using parameter values estimated in step7
  sim_after_step7 <- compute_simulations(
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
  ## Compute bias2 and rRMSE for each observed variable from default values of parameters, estimated values after step6 and estimated values after step7
  stats_default <- summary(sim_default$model_results$sim_list,
    obs = obs_list_transformed, stats = c("Bias2", "rRMSE", "EF")
  )
  stats_step6 <- summary(sim_after_step6$model_results$sim_list,
    obs = obs_list_transformed, stats = c("Bias2", "rRMSE", "EF")
  )
  stats_step7 <- summary(sim_after_step7$model_results$sim_list,
    obs = obs_list_transformed, stats = c("Bias2", "rRMSE", "EF")
  )
  ## Concatenate the different stats in a data.frame with a column variable, a column step and a column per stat
  stats_per_step <- data.frame(
    variable = obs_var_names,
    step = rep(c("default", "step6", "step7"), each = length(obs_var_names)),
    Bias2 = c(stats_default$Bias2, stats_step6$Bias2, stats_step7$Bias2),
    rRMSE = c(stats_default$rRMSE, stats_step6$rRMSE, stats_step7$rRMSE),
    EF = c(stats_default$EF, stats_step6$EF, stats_step7$EF)
  )
  ## Arrange it per variable name
  stats_per_step <- stats_per_step[order(stats_per_step$variable), ]

  ## Generate scatter plots from default values of parameters, estimated values after step6 and estimated values after step7
  p_default <- plot(sim_default$model_results$sim_list, obs = obs_list_transformed, type = "scatter")
  p_step6 <- plot(sim_after_step6$model_results$sim_list, obs = obs_list_transformed, type = "scatter")
  p_step7 <- plot(sim_after_step7$model_results$sim_list, obs = obs_list_transformed, type = "scatter")
  ## Save the plots in the out_dir
  save_plot_pdf(
    p_default,
    out_dir = out_dir,
    file_name = "scatter_plots_default"
  )
  save_plot_pdf(
    p_step6,
    out_dir = out_dir,
    file_name = "scatter_plots_after_step6"
  )
  save_plot_pdf(
    p_step7,
    out_dir = out_dir,
    file_name = "scatter_plots_after_step7"
  )

  # Print results
  cat(paste(
    "\nAgMIP Phase IV protocol: Graphs and results can be found in ",
    out_dir, "\n"
  ))

  # Return results
  res$final_values <- res_step7$final_values
  res$forced_param_values <- res_step7$forced_param_values
  res$obs_var_list <- res_step7$obs_var_list
  res$values_per_step <-
    data.frame(
      name = names(res_step7$final_values),
      default = sapply(names(res_step7$final_values), function(x) {
        param_info[[x]]$default
      }),
      step6 = res_step6$final_values,
      step7 = res_step7$final_values
    )
  res$stats_per_step <- stats_per_step
  res$scatter_plots <- list(
    default = p_default,
    step6 = p_step6,
    step7 = p_step7
  )
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
