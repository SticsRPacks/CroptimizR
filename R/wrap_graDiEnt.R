#' @title A wrapper for graDiEnt package
#'
#' @inheritParams optim_switch
#'
#' @return list with final_values, init_values, est_values, GE, min_crit_value
#' @importFrom graDiEnt optim_SQGDE GetAlgoParams
#' @keywords internal
#'
wrap_graDiEnt <- function(optim_options, param_info, crit_options) {

  if (is.null((ranseed <- optim_options$ranseed))) {
    ranseed <- NULL
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

  control_params <- do.call(graDiEnt::GetAlgoParams, optim_options)
  crit_options$tot_max_eval <- control_params$n_particles * control_params$n_iter

  start_time <- Sys.time()

  .trace_env <- new.env(parent = emptyenv())
  .trace_env$par_list  <- list()
  .trace_env$crit_list <- list()
  .trace_env$k <- 0L

  if (!is.null(ranseed)) set.seed(ranseed)
  range_bounds <- bounds$ub - bounds$lb
  ObjFun <- function(u) {
    u <- as.numeric(u)
    names(u) <- param_names
    x <- bounds$lb + u * range_bounds
    names(x) <- param_names
    val <- main_crit(x, crit_options = crit_options)
    .trace_env$k <- .trace_env$k + 1L
    .trace_env$par_list[[.trace_env$k]]  <- x
    .trace_env$crit_list[[.trace_env$k]] <- val
    return(val)
  }
  SQGDE <- tryCatch(
    graDiEnt::optim_SQGDE(
      ObjFun         = ObjFun,
      control_params = control_params
    ),
    error = function(e) {
      warning(sprintf("GraDiEnt failed: %s", e$message))
      NULL
    }
  )
  elapsed <- Sys.time() - start_time

  # Verify criterion value
  if (is.null(SQGDE) || is.null(SQGDE$solution)) {
    stop(
      "The value of the criterion is NA."
    )
  }

  u_sol <- SQGDE$solution
  names(u_sol) <- param_names

  final_values <- bounds$lb + u_sol * range_bounds
  names(final_values) <- param_names
  min_crit_value <- SQGDE$solution_weight
  if (is.null(min_crit_value)) {
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
      n_it <- dim(tr)[1]
      last_pop <- tr[n_it, , ]
      est_u <- as.matrix(last_pop)
      colnames(est_u) <- param_names

      # transformation
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
  trace_df <- NULL
  if (.trace_env$k > 0L) {
    n_pop <- control_params$n_particles
    k <- .trace_env$k
    pars_mat <- do.call(rbind, lapply(.trace_env$par_list, function(v) as.numeric(v)))
    colnames(pars_mat) <- param_names
    crit_vec <- as.numeric(unlist(.trace_env$crit_list))

    n_it_est <- ceiling(k / n_pop)
    iter <- rep(seq_len(n_it_est), each = n_pop)[seq_len(k)]
    ind  <- rep(seq_len(n_pop), times = n_it_est)[seq_len(k)]

    trace_df <- as.data.frame(pars_mat)
    trace_df$ind <- ind
    trace_df$iter <- iter
    trace_df$crit <- crit_vec
    trace_df$eval <- seq_len(k)
    trace_df$method <- "graDiEnt"
    trace_df$rep <- 1L
        }
  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = min_crit_value,
    SQGDE = SQGDE,
    trace_df = trace_df
  )

  return(res)
}
