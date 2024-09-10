#' @title Call the required parameter estimation method
#'
#' @param optim_method see description in estim_param
#' @param optim_options see description in estim_param
#' @param param_info see description in estim_param
#' @param crit_options List containing several arguments given to `estim_param`
#'  function: `param_names`, `obs_list`, `crit_function`, `model_function`,
#'  `model_options`, `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return prints, graphs and a list containing the results of the parameter
#' estimation, which content depends on the method used, all that saved in the
#' defined in `optim_options.path_results`
#'
#' @keywords internal
#'

optim_switch <- function(...) {

  # Store and save results at the end of the current optimization step
  if (nargs() > 2) {

    # Initialize res
    res <- list()
    flag_error <- FALSE

    on.exit({

      res$forced_param_values = crit_options$forced_param_values

      if (exists(".croptEnv")) {

        # Save results even in case parameter estimation crash
        res$obs_var_list <- .croptEnv$obs_var_list
        if (exists(".croptEnv$obs_var_list")) {
          rm("obs_var_list", envir = .croptEnv)
        }

        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
          if (exists(".croptEnv$params_and_crit")) {
            rm("params_and_crit", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 2) {
          res$sim_intersect <- .croptEnv$sim_intersect
          if (exists(".croptEnv$sim_intersect")) {
            rm("sim_intersect", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 3) {
          res$obs_intersect <- .croptEnv$obs_intersect
          if (exists(".croptEnv$obs_intersect")) {
            rm("obs_intersect", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 4) {
          res$sim <- .croptEnv$sim
          res$sim_transformed <- .croptEnv$sim_transformed
          if (exists(".croptEnv$sim_transformed")) {
            rm("sim_transformed", envir = .croptEnv)
          }
        }
      }

      if (!is.null(optim_options$path_results) & length(res) > 0) {
        save(res, file = file.path(
          optim_options$path_results,
          "optim_results.Rdata"
        ))
      }

      if (!flag_error) { # do not return in case of error,
        # otherwise error is not catched in tests
        return(res)
      } else if (length(res) > 0) {
        warning(paste(
          "An error occured during the parameter estimation process (see other error and warning messages). Partial results saved in",
          file.path(optim_options$path_results, "optim_results.Rdata")
        ))
      }
    })
  }

  arguments <- list(...)
  optim_method <- arguments$optim_method
  optim_options <- arguments$optim_options
  wrap_args <- within(arguments, rm("optim_method"))
  if (nargs() > 2) {
    param_info <- arguments$param_info
    crit_options <- arguments$crit_options
  }

  flag_unknown_method <- FALSE
  tryCatch(
    if (optim_method == "nloptr.simplex" || optim_method == "simplex") {
      res <- do.call(wrap_nloptr, wrap_args)
      if (nargs() > 2) {
        res$obs_var_list <- .croptEnv$obs_var_list
        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
        }
        res <- post_treat_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res,
          crit_options = crit_options
        )
        res$plots <- plot_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res
        )
        summary_frequentist(
          optim_options = optim_options, param_info = param_info,
          optim_results = res
        )
      }
    } else if (optim_method == "BayesianTools.dreamzs" ||
      optim_method == "dreamzs") {
      res <- do.call(wrap_BayesianTools, wrap_args)
      if (nargs() > 2) {
        res$obs_var_list <- .croptEnv$obs_var_list
        res$plots <- plot_bayesian(
          optim_options = optim_options,
          param_info = param_info, optim_results = res
        )
        res$gelman_diags <- summary_bayesian(
          optim_options = optim_options, param_info = param_info,
          optim_results = res
        )
      }
    } else if (optim_method == "optim") {
      res <- do.call(wrap_optim, wrap_args)
      if (nargs() > 2) {
        res$obs_var_list <- .croptEnv$obs_var_list
        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
        }
        res <- post_treat_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res,
          crit_options = crit_options
        )
        res$plots <- plot_frequentist(
          optim_options = optim_options,
          param_info = param_info, optim_results = res
        )
        summary_frequentist(
          optim_options = optim_options, param_info = param_info,
          optim_results = res
        )
      }
    } else {
      flag_unknown_method <- TRUE
    },
    error = function(cond) {
      warning(cond)
      flag_error <<- TRUE
    }
  )

  if (flag_unknown_method) {
    flag_error <- TRUE
    stop(paste0(
      "Unknown method ", optim_method,
      ", please choose between nloptr.simplex, BayesianTools.dreamzs and optim."
    ))
  }

  return(res)
}

utils::globalVariables(c(".croptEnv"))
