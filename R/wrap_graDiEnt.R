#' @title A wrapper for graDiEnt package
#'
#' @inheritParams optim_switch
#'
#' @return list with final_values, init_values, est_values, GE, min_crit_value
#' @importFrom graDiEnt optim_SQGDE GetAlgoParams
#' @keywords internal
wrap_graDiEnt <- function(optim_options, param_info, crit_options) {
  if(is.null((ranseed <- optim_options$ranseed))) {
    ranseed = NULL
  }
  if (is.null((reltol <- optim_options$reltol))) {
    optim_options$converge_crit <- "stdev"
  }
  if (is.null((n_params <- optim_options$n_params))) {
    stop(
      "Optim_options$n_params is mandatory, please define it (e.g., 10 * number of parameters)"
    )
  }
  if (is.null((trace <- optim_options$return_trace))) {
    optim_options$return_trace <- TRUE
  }
  optim_options$ranseed = NULL # ranseed is set to NULL because getAlgoParams doesn't regonise it
  algorithm <- "graDiEnt"

  # return requested information if only optim_options is given in argument
  if (nargs() == 1 && methods::hasArg(optim_options)) {
    init_nb <- if (!is.null(optim_options$n_particles)) {
      optim_options$n_particles
    } else {
      NA_integer_
    }
    return(list(
      package = "graDiEnt", family = "Global",
      method = "SQGDE", init_values_nb = init_nb
    ))
  }
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  control_params <- do.call(graDiEnt::GetAlgoParams,optim_options)

  crit_options$tot_max_eval <- control_params$n_particles * control_params$n_iter

  start_time <- Sys.time()

  if (!is.null(ranseed)) set.seed(ranseed)
  range_bounds <- bounds$ub - bounds$lb
  ObjFun <- function(u) {
    u <- as.numeric(u)
    names(u) <- param_names
    x <- bounds$lb + u * range_bounds
    names(x) <- param_names
    main_crit(x, crit_options = crit_options)
  }
  SQGDE <- tryCatch(
    graDiEnt::optim_SQGDE(
      ObjFun    = ObjFun,
      control_params = control_params
    ),
    error = function(e) { warning(sprintf("GraDiEnt failed: %s", e$message)); NULL }
  )
  elapsed <- Sys.time() - start_time

  # Verify criterion value
  if (is.null(SQGDE) || is.null(SQGDE$solution))  {
    stop(
      "The value of the criterion is NA."
    )
  }
  u_sol <- SQGDE$solution
  names(u_sol) <- param_names

  final_values <- bounds$lb + u_sol * range_bounds
  names(final_values) <- param_names
  min_crit_value <- SQGDE$solution_weight
  if (is.null(min_crit_value) ) {
        min_crit_value <- main_crit(final_values, crit_options)
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
  if (!is.null(SQGDE$particles_trace)) {
    tr <- SQGDE$particles_trace
    if (length(dim(tr)) != 3) {
      warning("Unexpected dimension for SQGDE$particles_trace; est_values set to final_values only.")
      est_values <- matrix(
        final_values,
        nrow = 1L,
        dimnames = list(NULL, param_names)
      )
    } else {
      n_it     <- dim(tr)[1]
      last_pop <- tr[n_it, , ]        # matrice n_particles × n_params (en u)
      est_u    <- as.matrix(last_pop)
      colnames(est_u) <- param_names

      # transformation vers l’échelle physique
      est_values <- sweep(est_u, 2, range_bounds, `*`)
      est_values <- sweep(est_values, 2, bounds$lb, `+`)
      colnames(est_values) <- param_names
    }
  } else {
    est_values <- matrix(
      final_values,
      nrow = 1L,
      dimnames = list(NULL, param_names)
    )
  }


  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = min_crit_value,
    SQGDE = SQGDE
  )

  return(res)
}

