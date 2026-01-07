#' @title Call the required parameter estimation method
#'
#' @param optim_method see description in estim_param
#' @param optim_options see description in estim_param
#' @param param_info see description in estim_param
#' @param crit_options List containing several arguments given to `estim_param`
#'  function: `param_names`, `obs_list`, `crit_function`, `model_function`,
#'  `model_options`, `param_info`, `transform_obs`, `transform_sim`, `out_dir`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return prints, graphs and a list containing the results of the parameter
#' estimation, which content depends on the method used, all that saved in
#' `crit_options$out_dir`.
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
      res$forced_param_values <- crit_options$forced_param_values
      if (!is.null(res$final_values)) {
        res$forced_param_values <- compute_eq_const(
          res$forced_param_values,
          res$final_values
        )
      }

      if (exists(".croptEnv") && !is.null(arguments$crit_options$info_level)) {
        # Save results even in case parameter estimation crash
        res$obs_var_list <- .croptEnv$obs_var_list
        if (exists("obs_var_list", where = .croptEnv)) {
          rm("obs_var_list", envir = .croptEnv)
        }
        res$obs_situation_list <- .croptEnv$obs_situation_list
        if (exists("obs_situation_list", where = .croptEnv)) {
          rm("obs_situation_list", envir = .croptEnv)
        }
        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
          if (exists("params_and_crit", where = .croptEnv)) {
            rm("params_and_crit", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 2) {
          res$sim_intersect <- .croptEnv$sim_intersect
          if (exists("sim_intersect", where = .croptEnv)) {
            rm("sim_intersect", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 3) {
          res$obs_intersect <- .croptEnv$obs_intersect
          if (exists("obs_intersect", where = .croptEnv)) {
            rm("obs_intersect", envir = .croptEnv)
          }
        }
        if (arguments$crit_options$info_level >= 4) {
          res$sim <- .croptEnv$sim
          res$sim_transformed <- .croptEnv$sim_transformed
          if (exists("sim_transformed", where = .croptEnv)) {
            rm("sim_transformed", envir = .croptEnv)
          }
        }
      }

      if (!is.null(crit_options$out_dir) & length(res) > 0) {
        save(res, file = file.path(
          crit_options$out_dir,
          "optim_results.Rdata"
        ))
      }

      if (!flag_error) { # do not return in case of error,
        # otherwise error is not catched in tests
        return(res)
      } else if (length(res) > 0) {
        warning(paste(
          "An error occured during the parameter estimation procedure (see other error and warning messages). Partial results saved in",
          file.path(crit_options$out_dir, "optim_results.Rdata")
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
        res$obs_situation_list <- .croptEnv$obs_situation_list
        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
        }
        res <- post_treat_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res,
          crit_options = crit_options
        )
        plot_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res,
          out_dir = crit_options$out_dir
        )
        summary_frequentist(
          optim_options = optim_options, param_info = param_info,
          optim_results = res,
          out_dir = crit_options$out_dir,
          indent = crit_options$indent
        )
      }
    } else if (optim_method == "BayesianTools.dreamzs" ||
      optim_method == "dreamzs") {
      res <- do.call(wrap_BayesianTools, wrap_args)
      if (nargs() > 2) {
        res$obs_var_list <- .croptEnv$obs_var_list
        res$obs_situation_list <- .croptEnv$obs_situation_list
        plot_bayesian(
          optim_options = optim_options,
          param_info = param_info, optim_results = res,
          out_dir = crit_options$out_dir
        )
        summary_bayesian(
          optim_options = optim_options, param_info = param_info,
          optim_results = res,
          out_dir = crit_options$out_dir,
          indent = crit_options$indent
        )
      }
    } else if (optim_method == "optim") {
      res <- do.call(wrap_optim, wrap_args)
      if (nargs() > 2) {
        res$obs_var_list <- .croptEnv$obs_var_list
        res$obs_situation_list <- .croptEnv$obs_situation_list
        if (arguments$crit_options$info_level >= 1) {
          res$params_and_crit <- dplyr::bind_rows(.croptEnv$params_and_crit)
        }
        res <- post_treat_frequentist(
          optim_options = optim_options,
          param_info = param_info,
          optim_results = res,
          crit_options = crit_options
        )
        plot_frequentist(
          optim_options = optim_options,
          param_info = param_info, optim_results = res,
          out_dir = crit_options$out_dir
        )
        summary_frequentist(
          optim_options = optim_options, param_info = param_info,
          optim_results = res,
          out_dir = crit_options$out_dir,
          indent = crit_options$indent
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
