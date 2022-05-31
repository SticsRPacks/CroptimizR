#' @title A wrapper for optim function
#'
#' @inheritParams optim_switch
#'
#' @return prints, graphs and a list containing:
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `est_values`, the vector of estimated values for optimized parameters
#' for all repetitions
#' `ind_min_crit`, the index of the repetition that lead to the lowest value
#' of the criterion
#' `optim`, the data structure returned by nloptr
#'
#' @keywords internal
#'

wrap_optim <- function(optim_options,param_info,crit_options) {

  if (is.null((nb_rep <- optim_options$nb_rep))) { nb_rep <- 5 }
  if (is.null((ranseed <- optim_options$ranseed))) { ranseed <- NULL }
  if (is.null((hessian <- optim_options$hessian))) { hessian <- FALSE }
  if (is.null((method <- optim_options$method))) { method <- "Nelder-Mead" }
  else if (toupper(method)=="SANN") {
    warning("SANN algorithm is not yet interfaced in CroptimizR for package optim. Nelder-Mead will be used.")
    method <- "Nelder-Mead"
  }
  if (is.null((maxit <- optim_options$control$maxit))) {
    if (method=="Nelder-Mead") maxit<-500
    else maxit <- 100
  }
  if (is.null((path_results <- optim_options$path_results))) { path_results <- getwd() }

  # return requested information if only optim_options is given in argument
  if (nargs()==1 & methods::hasArg(optim_options)) {
    return(list(package="optim", family="Frequentist",
                method=method, init_values_nb=nb_rep))
  }


  crit_options$tot_max_eval <- nb_rep*maxit
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  set.seed(ranseed)

  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  # Optim package switches the method to L-BFGS if bounds are provided since
  # they are not handled with other methods ...
  # as bounds are required in estim_param, we set them to [-Inf,Inf] (for graphs)
  # and we do not provide them to optim if method is not L-BFGS
  if (method!="L-BFGS-B" && method!="Brent") {
    bounds$lb<-rep(-Inf,nb_params)
    bounds$ub<-rep(Inf,nb_params)
  }

  # Run optim for each repetition
  optim <- vector("list",nb_rep)
  optim<-lapply(optim,function(x) {x<-list(value=NA,par=rep(NA,nb_params))})

  start_time <- Sys.time()
  for (irep in 1:nb_rep){

    crit_options$irep <- irep
    if (method=="L-BFGS-B" || method=="Brent") {
      try(optim[[irep]] <- stats::optim(par=as.numeric(init_values[irep,]), fn = main_crit,
                                        method=method,
                                        lower=bounds$lb, upper=bounds$ub,
                                        control = optim_options$control,
                                        hessian=hessian,
                                        crit_options=crit_options))
    } else {
      try(optim[[irep]] <- stats::optim(par=as.numeric(init_values[irep,]), fn = main_crit,
                                        method=method,
                                        control = optim_options$control,
                                        hessian=hessian,
                                        crit_options=crit_options))
    }

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    cat(sprintf('Working: %.2f%%. Estimated remaining time: %.2f %s\n', progress * 100, remaining, units(remaining)))

  }
  if (all(is.na(sapply(optim,function(x) x$value)))) {
    stop(paste("All",nb_rep,
               "repetitions of the parameter estimation lead to an error.",
               "\n   * Please look at warning messages."))
  } else if (any(is.na(sapply(optim,function(x) x$value)))) {
    warning(paste("Some repetitions of the parameter estimation aborted.",
                  "\n   * Please look at other warning messages for more details."))
  }


  # Get the estimated values
  est_values <- t(rbind(sapply(optim,`[[`,"par")))
  colnames(est_values) <- param_names

  # Which repetition has the smallest criterion
  ind_min_crit <- which.min(sapply(optim, function(x) {if (!is.null(x$value)) x$value}))

  # Store all criterion values
  crit <- sapply(optim, function(x) x$value)

  final_values <- est_values[ind_min_crit,]
  names(final_values) <- param_names
  res <- list(final_values = final_values,
              init_values = init_values,
              est_values = est_values,
              min_crit_value = optim[[ind_min_crit]]$value,
              ind_min_crit = ind_min_crit,
              crit_values=crit,
              optim = optim)

  return(res)

}
