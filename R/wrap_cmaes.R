#' Wrap CMA-ES using parma
#'
#' @inheritParams optim_switch
#'
#' @return list with final_values, init_values, est_values, GE, min_crit_value
#' @importFrom parma cmaes cmaes.control
#' @keywords internal
#' @export
wrap_cmaes <- function(optim_options, param_info = NULL, crit_options) {
  if (!requireNamespace("parma", quietly = TRUE)) {
    stop("the package 'parma' (not installed) becouse it required a dependencies that doesn't exist in the cluster")
  }
  if(is.null((ranseed <- optim_options$ranseed))) {
    ranseed <- NULL
  }
  n_params <- optim_options$n_params
  if (is.null(n_params)) {
    n_params <- length(param_info)
  }
  if (is.null((trace <- optim_options$return_trace))) {
    optim_options$return_trace <- TRUE
  }
  optim_options$ranseed <- NULL # ranseed is set to NULL because getAlgoParams doesn't regonise it
  algorithm <- "cmaes"

  if (is.null(param_info) || is.null(crit_options)) {

    return(list(
      package = "parma", family = "Global",
      method = "cmaes", init_values_nb = init_nb <- if (!is.null(optim_options$ctrl$options$PopSize)) optim_options$ctrl$options$PopSize else NA_integer_
    ))
  }
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  control_params <- optim_options$ctrl


  ctrl <- parma::cmaes.control()



  # if ctrl = list(options=..., CMA=...) existe
  if (!is.null(control_params) && is.list(control_params) &&
      (!is.null(control_params$options) || !is.null(control_params$CMA))) {

    if (!is.null(control_params$options)) {
      for (nm in names(control_params$options)) {
        ctrl$options[[nm]] <- control_params$options[[nm]]
      }
    }

    if (!is.null(control_params$CMA)) {
      for (nm in names(control_params$CMA)) {
        ctrl$CMA[[nm]] <- control_params$CMA[[nm]]
      }
    }

    # si l'utilisateur a fourni directement une liste d'options (sans options/CMA)
  } else if (!is.null(control_params) && is.list(control_params)) {
    for (nm in names(control_params)) {
      ctrl$options[[nm]] <- control_params[[nm]]
    }
  }

  if (!is.null(ctrl$options$PopSize) && !is.null(ctrl$options$MaxIter)){
    crit_options$tot_max_eval <- as.integer(ctrl$options$PopSize * ctrl$options$MaxIter)
    }

  start_time <- Sys.time()

  if (!is.null(ranseed)) set.seed(ranseed)
  trace_env <- new.env(parent = emptyenv())
  trace_env$eval <- 0L
  trace_env$crit <- numeric(0)
  trace_env$X <- list()
  keep_trace <- isTRUE(optim_options$return_trace)
  ObjFun <- function(x) {
      x <- as.numeric(x)
      names(x) <- param_names
      val <- main_crit(x, crit_options = crit_options)

      if (keep_trace) {
          trace_env$eval <- trace_env$eval + 1L
          trace_env$crit[trace_env$eval] <- val
          trace_env$X[[trace_env$eval]] <- x
        }
    return(val)
    }
  insigma <- optim_options$insigma
  if (is.null(insigma)) {
  # default value if it is not provided
  insigma <- rep(0.3, nb_params)
  }
  if (length(insigma) == 1L) insigma <- rep(insigma, nb_params)

  #because parma::cmaes expects a numeric vector for 'pars' (length = nb_params)
  x0 <- init_values

  # init_values can be a matrix/data.frame with several rows (nb_values x nb_params) our case data frame
  if (is.data.frame(x0)) x0 <- as.matrix(x0)

  if (is.matrix(x0)) {
    # take the first row as starting point
    x0 <- x0[1, , drop = TRUE]
  }

  x0 <- as.numeric(x0)
  names(x0) <- param_names


  if (length(x0) != nb_params) {
    x0 <- (bounds$lb + bounds$ub) / 2
    names(x0) <- param_names
  }

  parma_res <- tryCatch(
    parma::cmaes(
      pars    = x0,
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
  if (isTRUE(optim_options$debug)) {
      message("parma_res fields: ", paste(names(parma_res), collapse = ", "))
    }


  elapsed <- Sys.time() - start_time

  if (is.null(parma_res)) {
    stop("parma::cmaes failed (object NULL).")
  }

  sol <- NULL
  if (!is.null(parma_res$bestever$x)) {
      sol <- parma_res$bestever$x
    } else if (!is.null(parma_res$par)) {
        sol <- parma_res$par
      } else if (!is.null(parma_res$xbest)) {
          sol <- parma_res$xbest
        }
  if (is.null(sol)) {
      stop("Impossible to find a solution in the object sent by parma::cmaes().")
    }
  final_values <- as.numeric(sol)
  names(final_values) <- param_names

  # for post_treat_global_optim()
  min_crit_value <- if (!is.null(parma_res$bestever$f)) parma_res$bestever$f
  else main_crit(final_values, crit_options)


  cmaes_bestever_f <- if (!is.null(parma_res$bestever$f)) parma_res$bestever$f else NA_real_
  cmaes_objective  <- if (!is.null(parma_res$objective))  parma_res$objective  else NA_real_


  # Getting the estimated values
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
  if (keep_trace && trace_env$eval > 0L) {
      Xmat <- do.call(rbind, trace_env$X)
      colnames(Xmat) <- param_names

        trace_df <- as.data.frame(Xmat)
        trace_df$crit <- trace_env$crit
        trace_df$eval <- seq_len(trace_env$eval)
        lambda <- ctrl$options$PopSize
        if (is.null(lambda) || is.na(lambda))
          lambda <- 30L
        lambda <- as.integer(lambda)

        trace_df$iter <- ((trace_df$eval - 1L) %/% lambda) + 1L   # génération CMA-ES
        trace_df$ind  <- ((trace_df$eval - 1L) %%  lambda) + 1L  # individu dans la pop            # single trajectory
        trace_df$rep  <- 1L
        trace_df$method <- "cmaes"
      }

  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = min_crit_value,
    CMAES = parma_res,
    trace_df = trace_df,
    cmaes_bestever_f = cmaes_bestever_f,
    cmaes_objective  = cmaes_objective,
    counteval = if (!is.null(parma_res$counteval)) parma_res$counteval else NA_integer_,
    stopflag  = if (!is.null(parma_res$stopflag))  parma_res$stopflag  else NA_character_,
    out_dir = crit_options$out_dir

  )

  return(res)
}

