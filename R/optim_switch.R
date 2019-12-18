#' @title Call the required parameter estimation method
#'
#' @inheritParams estim_param
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#'
#' @return prints, graphs and a list containing the results of the parameter estimation.
#' which content depends on the method used.
#'   e.g. for Nelder meade simplex in nloptr, this list contains
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `est_values`, the vector of estimated values for optimized parameters
#' for all repetitions
#' `ind_min_crit`, the index of the repetition that lead to the lowest value
#' of the criterion
#' `nlo`, the data structure returned by nloptr
#'
#' @keywords internal
#'

optim_switch <- function(param_names, obs_list, crit_function, model_function, model_options = NULL, optim_method = "nloptr.simplex", optim_options = NULL, prior_information) {

  # TO DO LIST
  # - externalize nloptr
  #      o see if we create one function for nloptr or one per methods included
  #        in nloptr (depends on their genericity)
  # - add parameter normalization step

  # Normalize parameters
  # TO DO


  # CALL optim method

  if (is.null((nb_rep <- optim_options$nb_rep))) {
    nb_rep <- 1
  }
  if (is.null((xtol_rel <- optim_options$xtol_rel))) {
    xtol_rel <- 1e-5
  }
  if (is.null((maxeval <- optim_options$maxeval))) {
    maxeval <- 500
  }
  if (is.null((ranseed <- optim_options$ranseed))) {
    ranseed <- NULL
  }
  if (is.null((path_results <- optim_options$path_results))) {
    path_results <- getwd()
  }

  nb_params <- length(param_names)

  crit_options_loc <- list()
  crit_options_loc$param_names <- param_names
  crit_options_loc$obs_list <- obs_list
  crit_options_loc$crit_function <- crit_function
  crit_options_loc$model_function <- model_function
  crit_options_loc$model_options <- model_options
  crit_options_loc$prior_information <- prior_information

  bounds <- get_params_bounds(prior_information)
  user_init_values <- get_params_init_values(prior_information)

  # Sample initial values and include user provided ones
  init_values <- sample_params(prior_information, nb_rep, ranseed)
  for (param in param_names) {
    idx <- which(!is.na(user_init_values[, param]))
    init_values[idx, param] <- user_init_values[idx, param]
  }

  # Run nloptr for each repetition
  nlo <- list()
  start_time <- Sys.time()
  for (irep in 1:nb_rep) {
    nlo[[irep]] <- nloptr::nloptr(
      x0 = as.numeric(init_values[irep, ]), eval_f = main_crit,
      lb = bounds$lb, ub = bounds$ub,
      opts = list(
        "algorithm" = "NLOPT_LN_NELDERMEAD",
        "xtol_rel" = xtol_rel, "maxeval" = maxeval,
        "ranseed" = ranseed
      ),
      crit_options = crit_options_loc
    )

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    print(sprintf("Working: %.2f%%. ETA: %.2f", progress * 100, remaining))
  }

  # Get the estimated values
  est_values <- t(sapply(nlo, function(x) x$solution))

  # Which repetion has the smallest criterion
  ind_min_crit <- which.min(sapply(nlo, function(x) x$objective))

  # Graph and print the results
  grDevices::pdf(file = file.path(path_results, "EstimatedVSinit.pdf"), width = 9, height = 9)
  for (ipar in 1:nb_params) {
    graphics::plot(init_values[, ipar], est_values[, ipar],
      main = "Estimated vs Initial values of the parameters for different repetitions",
      graphics::text(init_values[, ipar], est_values[, ipar], pos = 1, col = "black"),
      xlim = c(bounds$lb[ipar], bounds$ub[ipar]),
      ylim = c(bounds$lb[ipar], bounds$ub[ipar]),
      xlab = paste("Initial value for", param_names[ipar]),
      ylab = paste("Estimated value for", param_names[ipar])
    )
    graphics::text(init_values[ind_min_crit, ipar], est_values[ind_min_crit, ipar],
      labels = ind_min_crit, pos = 1, col = "red"
    )
  }
  grDevices::dev.off()

  # pdf(file = file.path(path_results,"ConvergencePlots.pdf") , width = 9, height = 9)
  # for (ipar in 1:(nb_params+1)) {
  #   plot(init_values[,ipar], est_values[,ipar],
  #        main = "Estimated vs Initial values of the parameters for different repetitions",
  #        text(init_values[,ipar], est_values[,ipar], pos=1,col="black"),
  #        xlim = c(bounds$lb[ipar],bounds$ub[ipar]),
  #        ylim = c(bounds$lb[ipar],bounds$ub[ipar]),
  #        xlab = paste("Initial value for", param_names[ipar]),
  #        ylab = paste("Estimated value for", param_names[ipar]))
  #   text(init_values[ind_min_crit,ipar], est_values[ind_min_crit,ipar],
  #        labels = ind_min_crit, pos=1,col="red")
  # }
  # dev.off()


  # Save the results of nloptr
  save(nlo, file = file.path(path_results, "optim_results.Rdata"))

  # Display of parameters for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit, ipar]))
  }
  print(paste("Minimum value of the criterion :", nlo[[ind_min_crit]]$objective))

  # res=est_values[ind_min_crit,]
  # names(res)=param_names

  final_values <- est_values[ind_min_crit, ]
  names(final_values) <- param_names
  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    ind_min_crit = ind_min_crit,
    nlo = nlo
  )
  return(res)
}
