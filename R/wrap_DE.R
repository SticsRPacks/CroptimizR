#' @title A wrapper for DEoptim package
#'
#' @inheritParams optim_switch
#'
#' @return prints, graphs and a list containing:
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `est_values`, data frame of values of all population
#' `ind_min_crit`, the index of the repetition that lead to the lowest value
#' of the criterion
#' `DE`, the data structure returned by DE
#' `crit_values` the vector of criterion values for the all population
#' @importFrom DEoptim DEoptim
#' @keywords internal

wrap_DEoptim <- function(optim_options, param_info, crit_options) {
  if(is.null((ranseed <- optim_options$ranseed))) {
    ranseed = NULL
    }
  if (is.null((reltol <- optim_options$reltol))) {
    optim_options$reltol <- 1e-10
  }
  if (is.null((NP <- optim_options$NP))) {
    stop(
      "Optim_options$NP is mandatory, please define it (e.g., 10 * number of parameters)"
    )
  }
  if (is.null((trace <- optim_options$trace))) {
    optim_options$trace <- FALSE
  }
  optim_options$ranseed = NULL # ranseed is set to NULL because DEoptim.controll doesn't regonise it
  optim_options$storepopfrom <- 1
  optim_options$storepopfreq <- 1
  control <- do.call(DEoptim::DEoptim.control, optim_options)
  algorithm <- "DEoptim"

  # return requested information if only optim_options is given in argument
  if (nargs() == 1 & methods::hasArg(optim_options)) {
    return(list(
      package = "DEoptim", family = "Global",
      method = "DEoptim", init_values_nb = control$NP
    ))
  }
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)

  crit_options$tot_max_eval <- control$NP * control$itermax
  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)
  control$initialpop <- as.matrix(init_values)


  start_time <- Sys.time()

  if (!is.null(ranseed)) set.seed(ranseed)
  fn_de <- function(x) {
    main_crit(x, crit_options = crit_options)
  }
  DE <- tryCatch(
    DEoptim::DEoptim(
      fn    = fn_de,
      lower = bounds$lb,
      upper = bounds$ub,
      control = control
    ),
    error = function(e) { warning(sprintf("DEoptim failed: %s", e$message)); NULL }
  )
  elapsed <- Sys.time() - start_time

  # Verify criterion value
  if (is.na( DE$optim$bestval)) {
    stop(
      "The value of the criterion is NA."
    )
  }


  # Get the estimated values  ==> matrix of NP * nb_params
  est_values <- NULL
  est_values <- as.matrix(DE$member$pop)
  colnames(est_values) <- param_names
  if (ncol(est_values) == nb_params) {
    colnames(est_values) <- param_names
  }

  # Final solution
  final_values <- DE$optim$bestmem
  names(final_values) <- param_names

  trace_df <- NULL
  if (!is.null(DE$member$storepop)) {
    storepop  <- DE$member$storepop
    bestvalit <- DE$member$bestvalit
    itermax <- length(storepop)
    NP      <- nrow(storepop[[1]])
    df_list      <- vector("list", itermax)
    eval_counter <- 0
    for (it in seq_len(itermax)) {
      pop_it <- as.data.frame(storepop[[it]])
      colnames(pop_it) <- param_names
      pop_it$ind  <- seq_len(NP)   #
      pop_it$iter <- it
      pop_it$crit <- bestvalit[it]
      idx <- seq_len(NP)
      pop_it$eval <- eval_counter + idx
      eval_counter <- eval_counter + NP
      pop_it$rep    <- 1L
      pop_it$method <- "DEoptim"
      df_list[[it]] <- pop_it
      }
      trace_df <- dplyr::bind_rows(df_list)
    }

  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = DE$optim$bestval,
    DE = DE,
    trace_df = trace_df
  )

  return(res)
}
