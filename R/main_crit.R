#' @title main function for criterion to optimize
#'
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

    if (crit_options$info_level>=1) {

      satisfy_const <- TRUE
      if (exists("flag_const")) satisfy_const <- flag_const

      if (is.null(.croptEnv$params_and_crit)) {
        .croptEnv$params_and_crit <- vector("list", crit_options$tot_max_eval)
        .croptEnv$eval_count <- 1
      } else {
        .croptEnv$eval_count <- .croptEnv$eval_count + 1
      }

      .croptEnv$params_and_crit[[.croptEnv$eval_count]] <- dplyr::bind_cols(crit=crit,
                                                                            tibble::tibble(!!!param_values_ori),
                                                                            satisfy_const=satisfy_const)

      if (!is.null(crit_options$irep)) { ## this condition is there to detect frequentist methods ... should be changed for more robust test later ...

        if ((.croptEnv$eval_count == 1) || (crit_options$irep > .croptEnv$params_and_crit[[.croptEnv$eval_count-1]]$rep) ) {

          eval <- 1
          iter <- NA
          .croptEnv$last_iter <- 0
          .croptEnv$last_crit <- crit
          if (!is.na(crit)) {
            iter <- 1
            .croptEnv$last_iter <- iter
          }

        } else {

          eval <- .croptEnv$params_and_crit[[.croptEnv$eval_count-1]]$eval+1
          iter <- NA
          if (!is.na(crit) && (is.na(.croptEnv$last_crit) || crit < .croptEnv$last_crit)) {
            ## in the if above, is.na(.croptEnv$last_crit) is there in case crit==NA at the first evaluation
            iter <- .croptEnv$last_iter+1
            .croptEnv$last_iter <- iter
            .croptEnv$last_crit <- crit
          }

        }

        .croptEnv$params_and_crit[[.croptEnv$eval_count]] <-
          dplyr::bind_cols(tibble::tibble(rep=crit_options$irep,
                                  eval=eval,
                                  iter=iter),
                           .croptEnv$params_and_crit[[.croptEnv$eval_count]])

      }

    }

    if (crit_options$info_level>=2) {
      if (is.null(.croptEnv$sim_intersect)) {
        .croptEnv$sim_intersect <- vector("list", crit_options$tot_max_eval)
      }
      .croptEnv$sim_intersect[[.croptEnv$eval_count]] <- obs_sim_list$sim_list
    }

    if (crit_options$info_level>=3) {
      if (is.null(.croptEnv$obs_intersect)) {
        .croptEnv$obs_intersect <- vector("list", crit_options$tot_max_eval)
      }
      .croptEnv$obs_intersect[[.croptEnv$eval_count]] <- obs_sim_list$obs_list
    }

    if (crit_options$info_level>=4) {
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
      filename <- file.path(crit_options$path_results,paste0("debug_crit_NA.Rdata"))
      warning(paste("The optimized criterion has taken the NA value. \n  * Parameter values, obs_list and model results will be saved in",
                    filename,"for sake of debugging."))
      # just warns in this case. The optimization method may handle the problem which
      # may happend only for certain parameter values ...).
      save(param_values, obs_list, model_results, file=filename)
    }

  })

  # Initializations
  param_names <- crit_options$param_names
  obs_list <- crit_options$obs_list
  crit_function <- crit_options$crit_function
  model_function <- crit_options$model_function
  model_options <- crit_options$model_options
  param_info <- crit_options$param_info
  transform_obs <- crit_options$transform_obs
  transform_sim <- crit_options$transform_sim
  satisfy_par_const <- crit_options$satisfy_par_const
  var_names <- crit_options$var_names
  forced_param_values <- crit_options$forced_param_values

  names(param_values) <- param_names
  situation_names <- names(obs_list)
  nb_situations=length(situation_names)
  param_names_sl=get_params_names(param_info, short_list = TRUE)
  nb_params_sl=length(param_names_sl)
  crit <- NA
  model_results <- NA
  obs_sim_list <- NA

  # Denormalize parameters
  # TO DO

  # Transform parameters
  # TO DO

  # Handle the case of group of parameters (i.e. different values depending on the situations)
  param_values_ori <- param_values
  if  ("sit_list" %in% names(param_info[[1]])) {
    sit_names_param_info <- unique(unlist(lapply(param_info,function(x) {unlist(x$sit_list)})))
    param_values_df <- sapply(sit_names_param_info,
                              function(x) CroptimizR:::get_params_per_sit(param_info,x,param_values),
                              simplify=FALSE)
    param_values <- dplyr::bind_rows(param_values_df, .id="situation")
  }

  # Handle the parameters to force
  if (is.vector(param_values)) {
    param_values <- c(forced_param_values,param_values)
  } else if(!is.null(forced_param_values)) {
    param_values <- dplyr::bind_cols(tibble::tibble(!!!forced_param_values),param_values)
  }

  # Apply constraints on the parameters
  if (!is.null(satisfy_par_const)) {
    flag_const <- tryCatch(
      satisfy_par_const(param_values=param_values, model_options=model_options),
      error=function(cond) {
        message(paste("Caught an error while executing the user function for including
        constraints on estimated parameters (argument satisfy_par_const of estim_param function). \n
                 param_values=",paste(param_values,collapse=",")))
        print(cond)
        stop()
      })
    if (!flag_const) {
      crit_type<-crit_function()
      if (stringr::str_detect(crit_type,"ls")) {
        return(crit<-Inf)
      } else if (stringr::str_detect(crit_type,"log-likelihood")) {
        return(crit<--Inf)
      } else if (stringr::str_detect(crit_type,"likelihood")) {
        return(crit<-0)
      } else {
        warning("Unknown type for criterion (argument crit_function of estim_param): contraints on parameters will not be taken into account.")
      }
    }
  }


  sit_names <- situation_names
  if (is.null(var_names)) {
    var_names <- setdiff(unique(unlist(lapply(obs_list, names))),"Date")
    sit_var_dates_mask <- obs_list
  } else {
    # use var_names as given by the user in argument of estim_param
    # set sit_var_dates_mask to NULL so that it can not be used in place of var_names
    # to select variables
    sit_var_dates_mask <- NULL
  }

  # Call model function
  model_results <- NULL
  sim <- NULL
  try(model_results <- model_function(model_options = model_options,
                                  param_values = param_values,
                                  sit_names = sit_names,
                                  var_names = var_names,
                                  sit_var_dates_mask = sit_var_dates_mask))
  sim <- model_results


  # Check results, return NA if incorrect
  if (is.null(model_results) || (!is.null(model_results$error) && model_results$error)) {
    warning(paste("Error in model simulations for parameters values",paste0(param_values,collapse=",")))
    return(crit<-NA)
  }
  if (is.null(model_results$sim_list) || length(model_results$sim_list)==0) {
    warning(paste("Model wrapper returned an empty list for parameters values",paste0(param_values,collapse=",")))
    return(crit<-NA)
  }
  if (!is.sim(model_results$sim_list)) {
    warning("Format of results returned by the model wrapper is incorrect!")
    return(crit<-NA)
  }


  # Transform simulations
  if (!is.null(transform_sim)) {
    sim_transformed <- NULL
    model_results <- tryCatch(
      transform_sim(model_results=model_results, obs_list=obs_list, param_values=param_values,
                    model_options=model_options),
      error=function(cond) {
        message(paste("Caught an error while executing the user function for transforming
        simulations (argument transform_sim of estim_param function). \n
                 param_values=",paste(param_values,collapse=",")))
        print(cond)
        stop()
      })
    sim_transformed <- model_results
  }
  # Check results, return NA if incorrect
  if (is.null(model_results) || (!is.null(model_results$error) && model_results$error)) {
    warning("Error in transformation of simulation results.")
    return(crit<-NA)
  }
  if (is.null(model_results$sim_list) || length(model_results$sim_list)==0) {
    warning("Transformation of simulation results returned an empty list!")
    return(crit<-NA)
  }
  if (!is.sim(model_results$sim_list)) {
    warning("Format of results returned by transformation of model results is incorrect!")
    return(crit<-NA)
  }


  # Transform observations
  if (!is.null(transform_obs)) {
    obs_list <- tryCatch(
      transform_obs(model_results=model_results, obs_list=obs_list, param_values=param_values,
                    model_options=model_options),
      error=function(cond) {
        message(paste("Caught an error while executing the user function for transforming
        observations (argument transform_obs of estim_param function). \n
                 param_values=",paste(param_values,collapse=",")))
        print(cond)
        stop()
      })
  }
  # Check results, return NA if incorrect
  if ( is.null(obs_list) || !is.obs(obs_list) ) {
    warning("Transformation of observations returned an empty list or a list with an unexpected format.")
    return(crit<-NA)
  }


  # Intersect sim and obs
  obs_sim_list <- intersect_sim_obs(model_results$sim_list, obs_list)
  if (!is.list(obs_sim_list)) {
    warning("Intersection of simulations and observations is empty (no date and/or variable in common)!")
    return(crit<-NA)
  }
  if (any(sapply(obs_sim_list$sim_list,function(x) any(sapply(x,is.nan)))) || any(sapply(obs_sim_list$sim_list,function(x) any(sapply(x,is.infinite))))) {
    warning("The model wrapper returned NaN or infinite values: \n ",obs_sim_list$sim_list,"\n Estimated parameters: ",paste(param_names,collapse=" "),", values: ",paste(param_values, collapse=" "))
    return(crit<-NA)
  }


  # Filter reserved columns that should not be taken into account in the computation of the criterion
  obs_sim_list$sim_list <- sapply(obs_sim_list$sim_list, function(x) {x[ , !(names(x) %in% "Plant"),drop=FALSE]},
                                  simplify = F)
  obs_sim_list$obs_list <- sapply(obs_sim_list$obs_list, function(x) {x[ , !(names(x) %in% "Plant"),drop=FALSE]},
                                  simplify = F)

  # Check consistency of observations and simulations and make them consistent if possible
  obs_sim_list <- CroptimizR:::make_obsSim_consistent(obs_sim_list$sim_list,  obs_sim_list$obs_list)

  # Compute criterion value
  crit=crit_function(obs_sim_list$sim_list, obs_sim_list$obs_list)
  if (is.nan(crit)) {
    warning(paste0("Optimized criterion returned NaN value. \n Estimated parameters: ",paste(param_names,collapse=" "),", values: ",paste(param_values, collapse=" ")))
  }

  return(crit)
}
