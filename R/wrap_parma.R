#' @title A wrapper for parma::cmaes package (CMA-ES)
#'
#' @inheritParams optim_switch
#'
#' @return list with final_values, init_values, est_values, GE, min_crit_value
#' @importFrom parma cmaes cmaes.control
#' @keywords internal
#'
wrap_cmaes <- function(optim_options, param_info, crit_options) {
  if(is.null((ranseed <- optim_options$ranseed))) {
    ranseed = NULL
  }
  if (is.null((n_params <- optim_options$n_params))) {
    stop(
      "Optim_options$n_params is mandatory, please define it (e.g., 10 * number of parameters)"
    )
  }
  if (is.null((trace <- optim_options$return_trace))) {
    optim_options$return_trace <- TRUE
  }
  optim_options$ranseed <- NULL # ranseed is set to NULL because getAlgoParams doesn't regonise it
  algorithm <- "parma_cmaes"

  # return requested information if only optim_options is given in argument
  if (nargs() == 1 && methods::hasArg(optim_options)) {
    init_nb <- if (!is.null(optim_options$n_particles)) {
      optim_options$n_particles
    } else {
      NA_integer_
    }
    return(list(
      package = "parma", family = "Global",
      method = "cmaes", init_values_nb = init_nb
    ))
  }
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  control_params <- optim_options$ctrl
  if (is.null(control_params )) {
    control_params <- parma::cmaes.control()
   } else{
     if (is.null(control_params$options) && is.null(control_params$CMA)) {
       ctrl <- parma::cmaes.control(options = control_params, CMA = list())
       }else {
         ctrl <- do.call(parma::cmaes.control, control_params)
       }
   }
  if (!is.null(ctrl$options$PopSize) && !is.null(ctrl$options$MaxIter)){
    crit_options$tot_max_eval <- as.integer(ctrl$options$PopSize * ctrl$options$MaxIter)
    }

  start_time <- Sys.time()

  if (!is.null(ranseed)) set.seed(ranseed)
  ObjFun <- function(x){
    x <- as.numeric(x)
    names(x) <- param_names
    main_crit(x, crit_options = crit_options)
    }
  insigma <- optim_options$insigma
  if (is.null(insigma)) {
  # default value if it is not provided
  insigma <- rep(0.3, nb_params)
  }
  if (length(insigma) == 1L) insigma <- rep(insigma, nb_params)
  parma_res <- tryCatch(
    parma::cmaes(
    pars    = init_values,
    fun     = ObjFun,
    lower   = bounds$lb,
    upper   = bounds$ub,
    insigma = insigma,
    ctrl    = ctrl
    ),
    error = function(e) {
    warning(sprintf("parma::cmaes failed: %s", e$message))
     NULL
    }
    )
  elapsed <- Sys.time() - start_time

  if (is.null(parma_res)) {
    stop("parma::cmaes failed (object NULL).")
  }

  sol <- NULL
  if (!is.null(parma_res$par)) sol <- parma_res$par
  if (is.null(sol) && !is.null(parma_res$bestever$x))
    sol <- parma_res$bestever$x
  if (is.null(sol) && !is.null(parma_res$xbest))
    sol <- parma_res$xbest
  if (is.null(sol)) {
    stop("Impossible to find a solution in the object sent by parma::cmaes().")
    }

  final_values <- as.numeric(sol)
  names(final_values) <- param_names

  min_crit_value <- NULL
  if (!is.null(parma_res$value))
    min_crit_value <- parma_res$value
  if (is.null(min_crit_value) && !is.null(parma_res$bestever$f))
    min_crit_value <- parma_res$bestever$f
  if (is.null(min_crit_value)) {
    min_crit_value <- main_crit(final_values, crit_options = crit_options)
  }

  # Get the estimated values  ==> matrix of 1 * nb_params
  est_values <- matrix(
    final_values,
    nrow = 1L,
    dimnames = list(NULL, param_names)
  )
  colnames(est_values) <- param_names
  if (ncol(est_values) == nb_params) {
    colnames(est_values) <- param_names
  }

  # Final solution
  trace_df <- NULL

  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = min_crit_value,
    CMAES = parma_res,
    trace_df = trace_df
  )

  return(res)
}

